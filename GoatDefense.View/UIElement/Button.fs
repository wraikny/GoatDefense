[<AutoOpen>]
module GoatDefense.View.UIElement.Button

/// 形状を指定する型。
type Shape =
  | Rectangle of asd.Vector2DF
  | Circle of float32


/// Buttonにわたすためのパラメータ
type ButtonParameters =
  {
    text : string
    font : asd.Font
    backColor : asd.Color
    selectedColor : asd.Color
    position : asd.Vector2DF
    shape : Shape
  }

/// ボタンを表示するためのクラス。
[<Class>]
type Button(buttonParameters) =
  inherit asd.GeometryObject2D(
    // 形状を指定する。
    Shape = (
      buttonParameters.shape |> function
      | Rectangle(size) ->
        new asd.RectangleShape(DrawingArea = new asd.RectF(new asd.Vector2DF(0.0f, 0.0f), size))
        :> asd.Shape
      | Circle(radius) ->
        new asd.CircleShape(OuterDiameter = radius * 2.0f)
        :> asd.Shape
    ),
    Color = buttonParameters.backColor
  )

  let buttonParameters : ButtonParameters = buttonParameters

  /// テキストを子オブジェクトとして追加するメソッド。
  member private this.AddText() =
    let textObj =
      let bp = buttonParameters
      let size =
        buttonParameters.font.CalcTextureSize(
          bp.text, asd.WritingDirection.Horizontal
        )

      new asd.TextObject2D(
        Text = bp.text,
        Font = buttonParameters.font,
        Position = (
          buttonParameters.shape |> function
          | Rectangle(frameSize) ->
            (frameSize - size.To2DF()) / 2.0f
          | Circle(radius) ->
            -size.To2DF() / 2.0f
        )
      )

    // レイヤーの登録・オブジェクトの破棄・更新・描画を同期
    // 位置と変形をすべて同期
    // 子に親の描画優先度を加算する(親より手前に来る)
    this.AddDrawnChild(
      textObj,
      asd.ChildManagementMode.RegistrationToLayer
      ||| asd.ChildManagementMode.Disposal
      ||| asd.ChildManagementMode.IsUpdated
      ||| asd.ChildManagementMode.IsDrawn,
      asd.ChildTransformingMode.All,
      asd.ChildDrawingMode.DrawingPriority
    )

  member this.ColorIsSelected(cond : bool) =
    if cond then
      this.Color <- buttonParameters.selectedColor
    else
      this.Color <- buttonParameters.backColor

  override this.OnAdded() =
    this.AddText()


open System.Linq

/// 衝突判のあるボタンを表示するためのクラス。
[<Class>]
type CollidableButton<'Msg>(buttonParameters, msg) =
  inherit Button(buttonParameters)

  // IHasMsgインターフェースを実装。
  interface IHasMsg<'Msg> with
    member val Msg = msg with get

  /// 形状に応じたコライダーを作成して追加するメソッド。
  member private this.AddCollider() =
    let collider =
      buttonParameters.shape |> function
      | Rectangle(size) ->
        new asd.RectangleCollider(Area = new asd.RectF(new asd.Vector2DF(0.0f, 0.0f), size))
        :> asd.Collider2D

      | Circle(radius) ->
        new asd.CircleCollider(Radius = radius)
        :> asd.Collider2D

    // collider.IsVisible <- true

    this.AddCollider(collider)


  override this.OnAdded() =
    base.OnAdded()

    this.AddCollider()
[<AutoOpen>]
module GoatDefense.View.UIElement.Mouse

open System.Linq
open GoatDefense.Core.Helper


type MouseButton =
  | Left
  | Right


[<Class>]
// プライマリコンストラクタはinternalに制限
type CollidableMouse<'Model, 'Msg> internal (manager, camera, area, functiuons) =
  inherit asd.GeometryObject2D()

  let manager : Manager.Manager<'Model, 'Msg> = manager
  let camera : asd.CameraObject2D option = camera
  let area : asd.RectF = area

  /// 非衝突のクリックで実装するメソッドのリスト。
  let functions :
    (MouseButton *
      (Manager.Manager<'Model, 'Msg> ->
      CollidableMouse<'Model, 'Msg> -> unit)
    ) list =
    functiuons

  // マウスの位置が領域内にあるかどうかを調べる。
  let isInsideArea() : bool =
    let area =
      camera
      |> Option.map(fun c -> c.Dst.ToF())
      |> Option.defaultValue area

    let isOrderd a b c = a <= b && b <= c

    let areaRightDown = area.Position + area.Size

    let mousePos = asd.Engine.Mouse.Position

    (isOrderd area.Position.X mousePos.X areaRightDown.X ) &&
    (isOrderd area.Position.Y mousePos.Y areaRightDown.Y )


  // 追加オブジェクトコンストラクタ

  /// レイヤーにカメラを使用している場合のコンストラクタ
  new(manager, camera, functiuons) =
    let windowSize = asd.Engine.WindowSize.To2DF()
    new CollidableMouse<'Model, 'Msg>(
      manager, Some camera,
      new asd.RectF(new asd.Vector2DF(0.0f, 0.0f), windowSize),
      functiuons
    )

  /// カメラは使っていないがマウスの範囲を制限したい場合のコンストラクタ
  new(manager, area, functiuons) =
    let windowSize = asd.Engine.WindowSize.To2DF()
    new CollidableMouse<'Model, 'Msg>(
      manager, None, area, functiuons
    )

  /// 範囲を制限しない(画面全体)の場合のコンストラクタ
  new(manager, functiuons) =
    let windowSize = asd.Engine.WindowSize.To2DF()
    new CollidableMouse<'Model, 'Msg>(
      manager, new asd.RectF(new asd.Vector2DF(0.0f, 0.0f), windowSize), functiuons
    )


  // マウスの位置に合わせてオブジェクトを動かすためのメソッド。
  member private this.SetPosition() =
    // マウスの座標
    let mousePos = asd.Engine.Mouse.Position

    camera |> function
    | Some(camera) ->
      let src = camera.Src.ToF()
      let dst = camera.Dst.ToF()
      // カメラを使っているときは座標に補正をかける。
      let position = (mousePos - dst.Position) * src.Size / dst.Size + src.Position
      this.Position <- position

    | None ->
      this.Position <- mousePos


  /// 衝突状態にある'Msgをすべて取得
  member private this.GetMessages() : 'Msg list =
    // OfTypeをするためにLinqを用いている。

    // 衝突情報の一覧
    this.Collisions2DInfo
      // 自身が衝突したものを抽出
      .Where(fun x -> x.SelfCollider.OwnerObject.Equals(this))
      // 保持しているオブジェクトに変換
      .Select(fun x -> x.TheirsCollider.OwnerObject)
      // 描画されているオブジェクトを抽出
      .Where(fun x -> x.AbsoluteBeingDrawn)
      // インターフェースを実装しているものを抽出
      .OfType<IHasMsg<'Msg>>()
      // Msgに変換
      .Select(fun x -> x.Msg)

    |> Seq.toList


  // Msgのリストの先頭要素があればModelを更新する。
  member private this.FireMsg() =
    // エイリアス
    let isReleased = (=) asd.MouseButtonState.Release
    let left = isReleased asd.Engine.Mouse.LeftButton.ButtonState
    let right = isReleased asd.Engine.Mouse.RightButton.ButtonState

    let funcsWithButton button =
      let functions = functions |> List.filter (fst >> (=) button)
      for _, func in functions do
          func manager this

    // 左右のボタンの押し状態
    (left, right)
    |> function
    // 左のボタンが押されている
    | true, _ ->
      this.GetMessages() |> function
      // 発火しうるMsgが存在する場合
      | msg::_ ->
        // 更新
        manager.Update(msg)

      // 存在しない場合。
      | _ ->
        funcsWithButton Left

    // 右のボタンのみ押されている。
    | false, true -> funcsWithButton Right

    | _ -> ()

  // コライダを定義
  member val private Collider = new asd.CircleCollider( Radius = 5.0f )

  override this.OnAdded() =
    // コライダを追加
    this.AddCollider(this.Collider)


  override this.OnUpdate() =
    let inside = isInsideArea()

    // 範囲内にある場合のみ更新を行う。
    if inside then
      this.SetPosition()

      this.FireMsg()

    // Colliderの表示状態切替
    this.Collider.IsVisible <- inside

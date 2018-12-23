[<AutoOpen>]
module GoatDefense.View.Game.ActorView

open GoatDefense.Core.Helper
open GoatDefense.Core.Model
open GoatDefense.Core.Message
open GoatDefense.View.UIElement

[<Class>]
type ActorView(id, actor : Actor.Actor, gameConfig) as this =
  inherit asd.TextureObject2D ()
  // コンストラクタ
  do
    // filenameを取得する
    let filename =
      GameConfig.getActorFilename actor.actorType (!gameConfig)

    // 各プロパティをセットする。
    this.Texture <- asd.Engine.Graphics.CreateTexture2D(filename)
    this.CenterPosition <- this.Texture.Size.To2DF() / 2.0f
    this.Position <-
      let cdn = actor.coordinate
      new asd.Vector2DF(cdn.x, cdn.y)

    (!gameConfig).actorConfigMap
    |> Map.tryFind actor.actorType
    |> function
    | Some(ac) ->
      let size = this.Texture.Size.To2DF()
      this.Scale <-
        new asd.Vector2DF(1.0f, 1.0f) * float32 ac.texSize / (max size.X size.Y)
    | None -> ()


  // マウスでクリックするとオブジェクトを選択できるようにする。
  // こうすることで、CollidableButtonに対する処理と同一に記述できる。
  interface IHasMsg<Msg> with
    member val Msg = Msg.SelectActor(Some <| Model.Info id) with get


  /// オブジェクトの位置を更新するためのメソッド。
  member this.UpdatePosition(actor : Actor.Actor) =
    let cdn = actor.coordinate
    let pos = this.Position

    if Math.Vec2.init(pos.X, pos.Y) <> cdn then
      this.Position <- new asd.Vector2DF(cdn.x, cdn.y)


  override this.OnAdded() =
    // コライダを作る。
    let collider =
      new asd.RectangleCollider(
        Area =
          let size = this.Texture.Size.To2DF()
          in
          new asd.RectF(-size / 2.0f, size)
      )

    // オブジェクトにコライダを登録する。
    this.AddCollider(collider)
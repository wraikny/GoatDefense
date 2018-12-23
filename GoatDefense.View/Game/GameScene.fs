module GoatDefense.View.Game.GameScene


open GoatDefense.Core.Helper
open GoatDefense.Core.Helper.Manager
open GoatDefense.Core.Helper.Math
open GoatDefense.Core.Model
open GoatDefense.Core.Message
open GoatDefense.Core.Update
open GoatDefense.View.UIElement.Mouse

[<Class>]
type GameScene() =
  inherit asd.Scene()

  /// 設定
  let gameConfig =
    GameConfig.init(
      10,
      [
        {
          actorType = "WhiteGoat"
          filename = "Images/Actor/WhiteGoat.png"
          texSize = 100
        }
        {
          actorType = "BlackGoat"
          filename = "Images/Actor/BlackGoat.png"
          texSize = 100
        }
      ],
      "Images/Actor/NotFound.png"
    )
 
  /// Model管理
  let manager =
    let model =
      let player = Model.Player.init([])
      Model.Model.init(player)

    new Manager<Model.Model, Msg>(model, Model.update)


  override this.OnRegistered() =
    /// 背景のレイヤー。
    let backLayer =
      let layer = new asd.Layer2D()

      let windowSize = asd.Engine.WindowSize

      // 図形オブジェクト
      let gameBackColor =
        // 描画領域
        let drawingArea =
          GameConfig.getGameDrawingArea gameConfig

        // 矩形
        let rect =
          new asd.RectangleShape( DrawingArea = drawingArea.ToF() )
        new asd.GeometryObject2D(
          Shape = rect,
          Color = new asd.Color(84uy, 242uy, 101uy)
        )

      // 図形オブジェクト
      let uiBackColor =
        // 描画領域
        let drawingArea = GameConfig.getUIDrawingArea gameConfig

        // 矩形
        let rect =
          new asd.RectangleShape(DrawingArea = drawingArea.ToF() )
        new asd.GeometryObject2D(
          Shape = rect,
          Color =
            let b = 100uy in
            new asd.Color(b, b, b)
        )

      layer.AddObject(gameBackColor)
      layer.AddObject(uiBackColor)

      layer

    /// ActorViewを追加するためのレイヤー。
    let actorsLayer =
      let layer = new asd.Layer2D()

      // コンポーネントを追加する。
      let actorManager = new ActorManagerComponent(gameConfig, manager)
      layer.AddComponent(actorManager, "ActorManager")

      /// 描画領域を限定するためのカメラ
      let camera =
        let drawArea = GameConfig.getGameDrawingArea gameConfig

        new asd.CameraObject2D(
          Src = drawArea, // 描画元
          Dst = drawArea // 描画先
        )

      layer.AddObject(camera)

      let mouse =
        new CollidableMouse<Model.Model, Msg>(
          manager, camera,
          [
            // ModelのSelectedActorが追加状態の時、左クリックでAddActorを発火する。
            MouseButton.Left,
            (fun
              (m : Manager<Model.Model, Msg>)
              (cm : CollidableMouse<Model.Model, Msg>) ->
              m.Model().player.selectedActor
              |> function
              | Some (Model.SelectedActor.Add(_)) ->
                let cdn = Math.Vec2.init(cm.Position.X, cm.Position.Y)
                manager.Update(Msg.AddActor(cdn))
              | _ -> ()
            )

            // Modelが選択状態の時、右クリックで選択状態を解除する。
            MouseButton.Right,
            (fun (m : Manager<Model.Model, Msg>) _ ->
              manager.Update(Msg.SelectActor(None))
            )

          ]
        )

      layer.AddObject(mouse)

      layer

    /// ボタンや情報を表示するためのレイヤー。
    let uiLayer = new GameUILayer.GameUILayer(gameConfig, manager)


    let layers =
      [
        backLayer
        actorsLayer
        uiLayer :> asd.Layer2D
      ]

    for layer in layers do
      // レイヤーを追加する。
      this.AddLayer(layer)


  override this.OnUpdated() =
    // Modelの更新を行う。
    manager.Update(Msg.NoOps)
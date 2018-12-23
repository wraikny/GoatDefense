module GoatDefense.View.Game.GameUILayer

open GoatDefense.Core.Model
open GoatDefense.Core.Message
open GoatDefense.Core.Helper
open GoatDefense.Core.Helper.Manager
open GoatDefense.View.UIElement.Mouse
open GoatDefense.View.UIElement.Button


module Helper =
  /// childをparentの子オブジェクトに登録する関数。
  let addChild child (parent : asd.Object2D) =
    parent.AddChild(
      child,
      asd.ChildManagementMode.RegistrationToLayer
      ||| asd.ChildManagementMode.Disposal
      ||| asd.ChildManagementMode.IsUpdated
      ||| asd.ChildManagementMode.IsDrawn,
      asd.ChildTransformingMode.All
    )

  /// オブジェクトのサイズを元に並べたオブジェクトを
  /// 子オブジェクトとして持つオブジェクトを生成する関数。
  let getObjects margin (objects : asd.Object2D list) : asd.GeometryObject2D =
    let parent = new asd.GeometryObject2D()
    let rec appendObj sum (list : asd.Object2D list) =
      list |> function
      | [] -> ()
      | obj::tail ->
        obj |> function
        | :? asd.TextureObject2D as tex ->
          Some <| tex.Texture.Size.To2DF()
        | :? asd.TextObject2D as text ->
          text.Font.CalcTextureSize(
            text.Text, asd.WritingDirection.Horizontal
          ).To2DF() |> Some
        | :? asd.GeometryObject2D as geom ->
          geom.Shape |> function
          | :? asd.RectangleShape as rect ->
            Some <| rect.DrawingArea.Size
          | :? asd.CircleShape as circle ->
            Some <| circle.OuterDiameter * new asd.Vector2DF(1.0f, 1.0f)
          | _ -> None
        | _ -> None
        |> Option.map(fun size -> obj.Scale * size)
        |> function
        | None -> ()
        | Some(size) ->
          obj.Position <- new asd.Vector2DF(obj.Position.X, sum)
          parent |> addChild obj

          appendObj (sum + size.Y + margin) tail

    appendObj 0.0f objects

    parent

  let createTexture texSize filename =
    let tex = asd.Engine.Graphics.CreateTexture2D(filename)
    let size = tex.Size.To2DF()
    new asd.TextureObject2D(
      Texture = tex,
      Scale =
        new asd.Vector2DF(1.0f, 1.0f) * float32 texSize / (max size.X size.Y)
    )

  let black = new asd.Color(0uy, 0uy, 0uy)
  let white = new asd.Color(255uy, 255uy, 255uy)

  let createText font text =
    new asd.TextObject2D(Font = font, Text = text)

  let createLine width length =
    new asd.GeometryObject2D(
      Shape = new asd.RectangleShape(
        DrawingArea = new asd.RectF(0.0f, 0.0f, length, width)
      ),
      Color = black
    )


module GameUITab =
  open Helper
  let margin = 10.0f

  let texSize = 100

  let fontTitle =
    asd.Engine.Graphics.CreateDynamicFont(
      "Font/mplus-1c-light.ttf",
      25, black, 0, black
    )

  let fontText =
    asd.Engine.Graphics.CreateDynamicFont(
      "Font/mplus-1c-light.ttf",
      18, white, 0, white
    )

  let buttonText =
    asd.Engine.Graphics.CreateDynamicFont(
      "Font/mplus-1c-light.ttf",
      22, white, 0, white
    )

  let buttonSize (drawnArea: asd.RectI) =
    let w = float32 drawnArea.Size.X * 0.8f in
    new asd.Vector2DF(w, w * 0.25f)

  let buttonParam (gameConfig) text =
    let drawnArea = GameConfig.getUIDrawingArea gameConfig
    {
      text = text
      font = buttonText
      backColor = black
      selectedColor = white
      position = new asd.Vector2DF(0.0f, 0.0f)
      shape = Rectangle(buttonSize drawnArea)
    }

  let lineWidth = 1.5f

  let thinLine (drawnArea : asd.RectI) =
    createLine lineWidth <| float32(drawnArea.Size.X) * 0.8f

  
  /// Actorの情報を表示するためのクラス
  [<Class>]
  type ActorInfo(gameConfig) =
    inherit asd.GeometryObject2D()

    let gameConfig : GameConfig = gameConfig
    let drawnArea = GameConfig.getUIDrawingArea gameConfig

    let menuButton =
      new CollidableButton<Msg>(
        buttonParam gameConfig "BacktoMenu", Msg.ChageUIMode(Model.Menu)
      )

    let line = thinLine drawnArea

    let textureObj =
      new asd.TextureObject2D(
        Position =
          new asd.Vector2DF(
            0.0f,
            (buttonSize drawnArea).Y + lineWidth + 2.0f * margin
          )
      )

    let textObj = createText fontText ""


    let removeButton =
      new CollidableButton<Msg>(
        buttonParam gameConfig "Remove", Msg.RemoveActor
      )


    /// 画像を更新するメソッド。
    member private this.SetTexture(actorType: ActorType) =

      textureObj.Texture <-
        asd.Engine.Graphics.CreateTexture2D(
          gameConfig
          |> GameConfig.getActorFilename actorType
        )
      let size = textureObj.Texture.Size
      textureObj.Scale <-
        new asd.Vector2DF(1.0f, 1.0f) * float32 texSize / float32 (max size.X size.Y)


    /// テキストを更新するメソッド
    member private this.SetText(id : ID, actor : Actor.Actor) =
      let text =
        let cdn = actor.coordinate
        [
          sprintf "%A" actor.actorType
          sprintf "ID: %A\n" id
          sprintf "Pos: (%.1f, %.1f)" cdn.x cdn.y
        ] |> String.concat "\n"

      textObj.Text <- text


    /// 要素の位置を調整するメソッド。
    member private this.SetPosition() =
      let size = textureObj.Texture.Size.To2DF() * textureObj.Scale
      textObj.Position <-
        new asd.Vector2DF(
          textObj.Position.X,
          textureObj.Position.Y + size.Y + margin
        )
      let size =
        textObj.Font.CalcTextureSize(
          textObj.Text, asd.WritingDirection.Horizontal
        ).To2DF()

      removeButton.Position <-
        new asd.Vector2DF(
          removeButton.Position.X,
          textObj.Position.Y + size.Y + margin
        )


    /// 要素を更新するメソッド。
    member this.UpdateInfo(id : ID, actor : Actor.Actor) =
      this.SetTexture(actor.actorType)
      this.SetText(id, actor)
      this.SetPosition()


    override this.OnAdded() =
      this |> addChild menuButton
      this |> addChild line
      this |> addChild textureObj
      this |> addChild textObj
      this |> addChild removeButton


  let actorConfigList(gameConfig : GameConfig) =
    let drawnArea = GameConfig.getUIDrawingArea gameConfig

    gameConfig.actorConfigMap
    |> Map.toList
    |> List.collect(fun (_, ac) ->
      [
        createTexture  texSize ac.filename :> asd.Object2D
        (
          [
            sprintf "%A" ac.actorType
          ] |> String.concat "\n"
          |> createText fontText
        ) :> asd.Object2D
        new CollidableButton<Msg>(
          buttonParam gameConfig "Add", Msg.SelectActor(Some <| Model.Add(ac.actorType))
        ) :> asd.Object2D
        thinLine drawnArea :> asd.Object2D
      ]
    )
    |> List.append
      [
        new CollidableButton<Msg>(
          buttonParam gameConfig "BacktoMenu", Msg.ChageUIMode(Model.Menu)
        ) :> asd.Object2D
        thinLine drawnArea :> asd.Object2D
      ]
    |> getObjects margin


  let menuObj (gameConfig : GameConfig) =
    let drawnArea = GameConfig.getUIDrawingArea gameConfig
    [
      new CollidableButton<Msg>(
        buttonParam gameConfig "Add", Msg.ChageUIMode(Model.AddList)
      ) :> asd.Object2D
      thinLine drawnArea :> asd.Object2D
    ]
    |> getObjects margin


[<Class>]
type GameUILayer(gameConfig, manager) =
  inherit asd.Layer2D()

  // 設定
  let gameConfig : GameConfig = gameConfig

  // ゲームのModelを管理するクラス。
  let gameManager : Manager<Model.Model, Msg> = manager

  /// GameUIの表示領域
  let drawnArea = GameConfig.getUIDrawingArea gameConfig

  /// UIオブジェクトの位置の指定。
  let uiObjectPos =
    drawnArea.Position.To2DF() +
    new asd.Vector2DF(drawnArea.Size.To2DF().X * 0.1f, 15.0f)

  /// Actorを選択した時に表示する情報
  let actorInfo =
    let obj =
      new GameUITab.ActorInfo(gameConfig)
    obj.Position <- uiObjectPos
    obj


  let gameUITabs : (Model.UIMode * asd.GeometryObject2D) list =
    /// メニュー
    let menuObj =
      let obj =
        GameUITab.menuObj(gameConfig)
    
      obj.Position <- uiObjectPos

      obj


    /// 追加可能なアクターの一覧
    let actorConfigList =
      let obj =
        GameUITab.actorConfigList(gameConfig)
      obj.Position <- uiObjectPos

      obj
    
    [
      Model.Menu, menuObj
      Model.AddList, actorConfigList
      Model.ActorInfo, actorInfo :> asd.GeometryObject2D
    ]

  /// Objectの表示・更新状態を変更する。
  let changeObjectState (cond) (obj : asd.Object2D) =
    obj.IsDrawn <- cond
    obj.IsUpdated <- cond


  let DrawTab name =
    for n, t in gameUITabs do
      changeObjectState (n = name) t

      if n = name then
        t.Position <- uiObjectPos


  /// ボードテキスト
  let boardText =
    new asd.TextObject2D(
      Text = "",
      Font = GameUITab.fontTitle,
      Position = (
        let w = drawnArea.Size.To2DF().X in
        new asd.Vector2DF(0.15f, 0.08f) * w
      ),
      DrawingPriority = 101
    )

  let updateBoardText text =
    if boardText.Text <> text then
      boardText.Text <- text
      let size = 
        boardText.Font.CalcTextureSize(
          text, asd.WritingDirection.Horizontal
        ).To2DF()
      boardText.Position <-
        new asd.Vector2DF(
          float32 drawnArea.Width / 2.0f - size.X / 2.0f,
          boardText.Position.Y
        )


  /// ボード
  let board =
    let tex = asd.Engine.Graphics.CreateTexture2D("Images/UI/BoardGoat.png")
    let size = tex.Size.To2DF()
    let scale = float32 drawnArea.Size.X / size.X
    let obj =
      new asd.TextureObject2D(
        Texture = tex,
        Scale = new asd.Vector2DF(scale,scale),
        Position =
          (drawnArea.Position + drawnArea.Size).To2DF() - size * scale,
        DrawingPriority = 100
      )

    obj.AddChild(
      boardText,
      asd.ChildManagementMode.RegistrationToLayer
      ||| asd.ChildManagementMode.Disposal
      ||| asd.ChildManagementMode.IsUpdated
      ||| asd.ChildManagementMode.IsDrawn,
      asd.ChildTransformingMode.Position
    )

    obj


  override this.OnAdded() =
    let mouse =
      new CollidableMouse<Model.Model, Msg>(
        gameManager, drawnArea.ToF(), []
      )

    this.AddObject(mouse)

    for _, t in gameUITabs do
      this.AddObject(t)
      changeObjectState false t

    this.AddObject(board)


  override this.OnUpdated() =
    let player = gameManager.Model().player
    DrawTab player.uiMode

    player.selectedActor |> function
    | Some(Model.Info(id)) ->
      player.actors
      |> Seq.tryAssoc id
      |> function
      | Some(actor) ->
        actorInfo.UpdateInfo(id, actor)
      | None -> ()

    | Some(Model.Add(ac)) ->
      updateBoardText <| sprintf "%s" ac

    | None -> ()


    player.uiMode |> function
    | Model.Menu-> updateBoardText "Menu"
    | Model.ActorInfo -> updateBoardText "Info"
    | Model.AddList ->
      if player.selectedActor = None then
        updateBoardText "Create"
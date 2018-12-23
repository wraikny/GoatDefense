module GoatDefense.View.Program

[<EntryPoint>]
let main _ =
  let w = 800
  let h = w / 4 * 3
  asd.Engine.Initialize("GoatDefense", w, h, new asd.EngineOption())
  |> ignore

  #if DEBUG
  asd.Engine.File.AddRootDirectory("Resources")
#else
  asd.Engine.File.AddRootPackageWithPassword("Resources.pack", "password")
#endif

  let scene = new Game.GameScene.GameScene()
  asd.Engine.ChangeScene(scene)

  while asd.Engine.DoEvents() do
    asd.Engine.Update()

  asd.Engine.Terminate()

  0
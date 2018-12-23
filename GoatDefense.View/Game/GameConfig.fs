[<AutoOpen>]
module GoatDefense.View.Game.GameConfig

open GoatDefense.Core.Helper

type ActorConfig =
  {
    actorType : ActorType
    filename : string
    texSize : int
  }

type GameConfig =
  {
    /// 描画領域のマージン
    drawAreaMargin : int
    actorConfigMap : Map<ActorType, ActorConfig>
    actorNotFoundImage : string
  }

module GameConfig =
  let init(drawAreaMargin, actorConfigs : ActorConfig list, actorNotFound) =
    {
      drawAreaMargin = drawAreaMargin
      actorConfigMap =
        actorConfigs
        |> List.map(fun ac -> (ac.actorType, ac))
        |> Map.ofList

      actorNotFoundImage = actorNotFound
    }

  /// ActorTypeからfilenameを取得する関数
  let getActorFilename (ac : ActorType) (gameConfig : GameConfig) =
    gameConfig.actorConfigMap
    |> Map.tryFind ac
    |> Option.map(fun x -> x.filename)
    |> Option.defaultValue(gameConfig.actorNotFoundImage)


  // ゲーム画面の描画領域を計算する関数。
  let getGameDrawingArea (gameConfig : GameConfig) : asd.RectI =
    let margin = gameConfig.drawAreaMargin
    let size = asd.Engine.WindowSize.Y - margin * 2
    new asd.RectI(margin, margin, size, size)

  // UI画面の描画領域を計算する関数。
  let getUIDrawingArea (gameConfig : GameConfig) : asd.RectI =
    let margin = gameConfig.drawAreaMargin
    let windowSize = asd.Engine.WindowSize
    let height = windowSize.Y
    let width = windowSize.X - height
    new asd.RectI(height, margin, width - margin, height - 2 * margin)
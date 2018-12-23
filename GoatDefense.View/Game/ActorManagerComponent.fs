[<AutoOpen>]
module GoatDefense.View.Game.ActorManagerComponent

open System.Collections.Generic

open GoatDefense.Core.Helper
open GoatDefense.Core
open GoatDefense.Core.Model

[<Class>]
type ActorManagerComponent(gameConfig, manager) =
  inherit asd.Layer2DComponent()

  // 設定
  let gameConfig : GameConfig = gameConfig

  /// Managerクラスへの参照。
  let manager : Manager.Manager<Model.Model, Message.Msg> = manager

  /// Playerを取得する関数
  let getPlayer() = manager.Model().player

  /// Actorが追加されたか比較するための変数。
  let mutable nextID : ID = getPlayer().nextID


  /// IDとActorのViewへの参照のペアを保持する
  member val private ActorsViewList = new List<ID * ActorView>()


  /// ActorViewを追加するメソッド。
  member private this.AddActorsViewList(actorsMap : Map<ID, Actor.Actor> ref) =
    // a..bはaとbを含むので、-1する
    for id in nextID..(getPlayer().nextID - ID.one) do
      !actorsMap
      |> Map.tryFind id
      |> function
      | Some(actor) ->
        let obj = new ActorView(id, actor, ref gameConfig)

        // ObjectのListに追加。
        this.ActorsViewList.Add( (id, obj) )
        // Layerに追加
        this.Owner.AddObject obj

      | None -> ()

    // 自身のnextIDを更新
    nextID <- getPlayer().nextID


  /// ActorViewを更新するメソッド。
  member private this.UpdateActorsView(actorsMap : Map<ID, Actor.Actor> ref) =
    // ActorsViewListを複製する。
    let objects = new List<ID * ActorView>(this.ActorsViewList)

    for (id, obj) in objects do
      !actorsMap
      |> Map.tryFind id
      |> function
      // ModelにIDが存在するとき
      | Some(actor) ->
        obj.UpdatePosition(actor)

      // ModelにIDが見つからないとき
      | None ->
        this.ActorsViewList.Remove( (id, obj) ) |> ignore
        obj.Dispose()


  override this.OnLayerUpdated() =
    // Modelのactorsを辞書に変換
    let actorsMap = getPlayer().actors |> Map.ofList

    // nextIDが異なるとき、オブジェクトの追加処理。
    if nextID <> getPlayer().nextID then
      this.AddActorsViewList(ref actorsMap)

    // 更新処理
    this.UpdateActorsView(ref actorsMap)
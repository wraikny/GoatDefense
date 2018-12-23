module GoatDefense.Core.Model.Model

open GoatDefense.Core.Helper
open GoatDefense.Core.Helper.Math
open GoatDefense.Core.Model.Actor


/// 選択するActorの種類
type SelectedActor =
  /// 新しいActorを追加する際に保持する。
  | Add of ActorType
  /// すでにあるオブジェクトの情報を閲覧する際に保持する。
  | Info of ID


type UIMode =
  | Menu
  | AddList
  | ActorInfo


type Player =
  {
    /// IDが被らないように、次に登録するIDを記録する。
    nextID : ID
    /// 選択中のActorの情報。
    selectedActor : SelectedActor option
    /// ActorがIDと一緒に格納される
    actors : (ID * Actor) list
    uiMode : UIMode
  }

module Player =
  let init(actors) =
    {
      nextID = actors |> List.length
      selectedActor = None
      actors = actors
      uiMode = Menu
    }
  /// ActorのIDを元に具体的なActorの情報を取得する。
  /// 失敗したらNoneで返す。
  let getActor (id : ID) (player : Player) : Actor option =
    player.actors
    |> Seq.tryAssoc id


/// ゲーム内のすべての情報を持つ型。
type Model =
  {
    /// ゲームが始まってからのフレーム数を表す。
    count : Count
    player : Player
  }


module Model =
  let init(player : Player) : Model =
    {
      count = Count.zero
      player = player
    }

  /// TeamとActorのIDを元に具体的なActorの情報を取得する。
  /// 失敗したらNoneで返す。
  let getActor (id : ID) (model : Model) : Actor option =
    model.player.actors
    |> Seq.tryAssoc id
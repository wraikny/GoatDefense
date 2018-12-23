module GoatDefense.Core.Model.Actor

open GoatDefense.Core.Helper
open GoatDefense.Core.Helper.Math


/// キャラクタ毎の情報を持つ型。
type Actor =
  {
    // lv : StatusValue
    actorType : ActorType
    coordinate : Coordinate
  }


module Actor =
  let init
    (actorType : ActorType)
    (cdn : Coordinate)
    : Actor =
    {
      actorType = actorType
      coordinate = cdn
    }
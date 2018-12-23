module GoatDefense.Core.Message

open GoatDefense.Core.Helper
open GoatDefense.Core.Model.Actor
open GoatDefense.Core.Model.Model


/// ユーザの操作の種類を更新処理に伝えるための型。
type Msg =
  | NoOps
  | SelectActor of SelectedActor option
  | AddActor of Coordinate
  | RemoveActor
  | ChageUIMode of UIMode
module GoatDefense.Core.Helper.Cmd

/// 副作用を扱うための型。
type Cmd<'Msg> =
  | Nothing
  | Random of (float -> 'Msg)
  | RandomList of uint32 * (float list -> 'Msg)

module Cmd =
  /// Cmdに副作用を込めてMsgに変換する関数。
  /// 新しいMsgが必要ないならNoneを返す。
  let toMsg<'Msg> (cmd : Cmd<'Msg>) : 'Msg option =
    match cmd with
      | Cmd.Nothing ->
        None

      | Random msg ->
        let rand = new System.Random()
        rand.NextDouble()
        |> msg
        |> Some

      | RandomList (n, msg) ->
        let rand = new System.Random()
        [for _ in 1u..n -> rand.NextDouble()]
        |> msg
        |> Some

module GoatDefense.Core.Helper.Manager

open GoatDefense.Core.Helper.Cmd

[<Class>]
type Manager<'Model, 'Msg>(initialModel, updateFunc) =
  let mutable model : 'Model = initialModel
  let updateFunc : 'Msg -> 'Model -> 'Model * Cmd<'Msg> = updateFunc

  member __.Model() = model

  member __.Update(msg : 'Msg) =
    /// Cmdから'Msg optionを作って再帰的に更新を行う。
    let rec update msg model =
      let model, cmd = updateFunc msg model
      match Cmd.toMsg(cmd) with
        | Some(msg) ->
          update msg model
        | None ->
          model

    /// 受け取った'Msgを使用して更新を行う。
    model <- update msg model
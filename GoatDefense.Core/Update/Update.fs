module GoatDefense.Core.Update

open GoatDefense.Core.Helper
open GoatDefense.Core.Model.Actor
open GoatDefense.Core.Model.Model
open System.Diagnostics

module Actor =
  let move (model : Model ref) (actor : Actor) : Actor =
    actor


  let update (model : Model ref) (actor : Actor) : Actor option =
    // 今後HPを導入して、死亡していた場合はNoneを返す
    actor
    |> move model
    |> Some


module Player =
  let updateActors (model : Model ref) (player : Player) : Player =
    // Listに対する処理を行うための関数定義。
    let f (actors : (ID * Actor) list) : (ID * Actor) list =
      actors
      // 関数を適用してNoneでない値のみ抽出する
      |> Seq.filterMap(
        fun (id, actor) ->
          Actor.update model actor
           // Noneでなかったらidとセットにする（タプルを作る）
          |> Option.map(fun actor -> id, actor)
      )
      // シーケンスからリストを構成する
      |> List.ofSeq

    // 部分的に変更した新しいModelを返す。
    { player with
        actors = player.actors |> f
    }

  let update (model : Model ref) (player : Player) : Player =
    player
    |> updateActors model


  let selectActor (selected : SelectedActor option) (player : Player) : Player =
    { player with selectedActor = selected }


  let addActor (coordinate : Coordinate) (player : Player) : Player =
    player.selectedActor |> function
    | Some(Add(actorType)) ->
      let actor =
        {
          actorType = actorType
          coordinate = coordinate
        }

      { player with
          // IDを更新
          nextID = player.nextID + ID.one
          // :: でリストの先頭に要素を追加
          actors = (player.nextID, actor)::player.actors
      }

    | _ -> player

  /// Playerが選択中のActorを削除する関数。
  let removeActor
    (player : Player) : Player =
    player.selectedActor |> function
    /// 選択中
    | Some((Info(id))) ->
      /// 再帰的にリストを辿り、該当するIDを除いたリストを返す関数。
      let rec remove id
        left (list : (ID * Actor) list)
        : (ID * Actor) list =
        match list with
        // IDが一致した場合。
        | x::right when fst x = id -> left @ right

        // IDが異なる場合。
        | x::right ->
          // 再帰。
          remove id (x::left) right

        // すべての要素を精査した場合。
        | [] -> left

      { player with
          // 未選択状態に戻す
          selectedActor = None
          actors = player.actors |> remove id []
      }

    /// それ以外(任意のパターンに一致する)
    | _ -> player


  let updateUIMode (uiMode : UIMode) (player : Player) =
    { player with uiMode = uiMode }


module Model =
  open GoatDefense.Core.Helper.Cmd
  open GoatDefense.Core.Message


  /// ModelのCountを1増やす。
  let countUp (model : Model) : Model =
    { model with count = model.count + Count.one }


  let updatePlayer (f : Player -> Player) (model : Model) : Model =
    // 部分的に変更した新しいModelを返す。
    { model with
        player = f model.player
    }



  /// Modelを更新する。
  /// Msg毎に異なる処理を行う。
  let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =

    #if DEBUG
    if msg <> Msg.NoOps then
      // デバッグ用文字出力
      printfn "%A is Fired!" msg
    #endif

    match msg with
      | Msg.NoOps ->
        let model =
          model
          |> countUp
          |> updatePlayer (Player.update <| ref model)

        ( model, Cmd.Nothing )

      | Msg.SelectActor(selected) ->
        let model =
          model
          |> updatePlayer (Player.selectActor selected)
          |> updatePlayer (
            selected |> function
            | Some(Info _) -> ActorInfo
            | Some(Add _) -> AddList
            | None -> Menu
            |> Player.updateUIMode
          )


        ( model, Cmd.Nothing )

      | Msg.AddActor(cdn) ->
        let model =
          model
          |> updatePlayer (Player.addActor cdn)

        ( model, Cmd.Nothing )

      | Msg.RemoveActor ->
        let model =
          model
          |> updatePlayer Player.removeActor
          |> updatePlayer (Player.updateUIMode Menu)

        ( model, Cmd.Nothing )

      | Msg.ChageUIMode(mode) ->
        let model =
          { model with
              player =
                { model.player with
                    uiMode = mode
                }
          }

        ( model, Cmd.Nothing )
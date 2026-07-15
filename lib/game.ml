open! Core
open Types

type t =
  { current : game_state
  ; history : game_state list
  ; start : player_state
  }

let create ~wall ~start =
  { current =
      { player = start; wall; broken_holds = Set.empty (module Int); status = Playing }
  ; history = []
  ; start
  }
;;

let current_state t = t.current

(* Status after a turn (§4.3 / §4.9):
   - stamina at 0 in a Strained/Critical pose (or with the grace turn
     disabled) -> Fallen;
   - stamina at 0 while Stable -> "pumped out": one grace chance — rest if a
     hand is already on a Rest hold, otherwise the next attempt is desperate;
   - win: both hands on Finish holds, a foot attached, stamina > 0. *)
let compute_status (gs : game_state) =
  let stability = Balance.stability gs.wall gs.player.limbs ~torso:gs.player.torso in
  let hand_on_finish id =
    match Option.bind id ~f:(Wall.find gs.wall) with
    | Some { kind = Finish; _ } -> true
    | Some _ | None -> false
  in
  let foot_attached =
    Option.is_some gs.player.limbs.left_foot || Option.is_some gs.player.limbs.right_foot
  in
  if gs.player.stamina <= 0
  then (
    match stability with
    | Stable when Config.exhaustion_grace_turn -> gs.status
    | Stable -> Fallen "exhausted: no strength left"
    | Strained | Critical | Falling ->
      Fallen
        (sprintf
           !"exhausted in a %{sexp:stability} pose"
           stability))
  else if hand_on_finish gs.player.limbs.left_hand
          && hand_on_finish gs.player.limbs.right_hand
          && foot_attached
  then Won
  else gs.status
;;

let move t limb ~hold_id =
  let gs = t.current in
  match Wall.find gs.wall hold_id with
  | None -> `Rejected Hold_broken
  | Some hold ->
    (match
       Movement.preview_move ~wall:gs.wall ~broken:gs.broken_holds gs.player limb hold
     with
     | Error reason -> `Rejected reason
     | Ok (player, cost) ->
       if cost.total > gs.player.stamina
       then (
         (* The desperate move: physically possible, but the tank is empty —
            the grip gives out mid-move. *)
         let reason =
           sprintf
             "grip gave out reaching hold %d (needed %d stamina, had %d)"
             hold_id
             cost.total
             gs.player.stamina
         in
         `Fell
           { t with
             current = { gs with status = Fallen reason }
           ; history = gs :: t.history
           })
       else (
         let next = { gs with player } in
         let next = { next with status = compute_status next } in
         `Moved ({ t with current = next; history = gs :: t.history }, cost)))
;;

let rest t =
  let gs = t.current in
  match Movement.attempt_rest ~wall:gs.wall ~broken:gs.broken_holds gs.player with
  | Error reason -> `Rejected reason
  | Ok player ->
    let next = { gs with player } in
    let next = { next with status = compute_status next } in
    `Rested { t with current = next; history = gs :: t.history }
;;

let undo t =
  match t.history with
  | [] -> None
  | previous :: history -> Some { t with current = previous; history }
;;

let reset t = create ~wall:t.current.wall ~start:t.start

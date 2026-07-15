open! Core
open Types

type t =
  { current : game_state
  ; history : game_state list
  ; start : player_state
  }

let create ~wall ~start =
  { current =
      { player = start
      ; wall
      ; broken_holds = Set.empty (module Int)
      ; hold_wear = Map.empty (module Int)
      ; status = Playing
      }
  ; history = []
  ; start
  }
;;

(* §4.3 crumbling holds: one turn has passed — every crumbling hold with a
   limb on it wears by 1; reaching its durability breaks it (added to
   broken_holds, drawn no more) and every limb on it DETACHES. Returns the
   ids that broke this turn. This is a world event, not a move, so it lives
   here rather than in the gate; the solver stays consistent by treating
   crumbling holds as unusable from the start (see solver.ml). *)
let tick_crumbling (gs : game_state) =
  let occupied_crumbling =
    List.filter_map (Player.attached gs.player.limbs) ~f:(fun (_, id) ->
      match Wall.find gs.wall id with
      | Some { kind = Crumbling; durability = Some d; _ } -> Some (id, d)
      | Some _ | None -> None)
    |> List.dedup_and_sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
  in
  List.fold occupied_crumbling ~init:(gs, []) ~f:(fun (gs, broke) (id, durability) ->
    let wear = 1 + Option.value (Map.find gs.hold_wear id) ~default:0 in
    if wear >= durability
    then (
      let detach limb_hold =
        match limb_hold with
        | Some h when h = id -> None
        | Some _ | None -> limb_hold
      in
      let limbs =
        { left_hand = detach gs.player.limbs.left_hand
        ; right_hand = detach gs.player.limbs.right_hand
        ; left_foot = detach gs.player.limbs.left_foot
        ; right_foot = detach gs.player.limbs.right_foot
        }
      in
      ( { gs with
          broken_holds = Set.add gs.broken_holds id
        ; hold_wear = Map.remove gs.hold_wear id
        ; player = { gs.player with limbs }
        }
      , id :: broke ))
    else { gs with hold_wear = Map.set gs.hold_wear ~key:id ~data:wear }, broke)
;;

let current_state t = t.current

(* The win predicate (§4.3) — also the solver's goal test. *)
let won ~(wall : wall) (player : player_state) =
  let hand_on_finish id =
    match Option.bind id ~f:(Wall.find wall) with
    | Some { kind = Finish; _ } -> true
    | Some _ | None -> false
  in
  let foot_attached =
    Option.is_some player.limbs.left_foot || Option.is_some player.limbs.right_foot
  in
  hand_on_finish player.limbs.left_hand
  && hand_on_finish player.limbs.right_hand
  && foot_attached
  && player.stamina > 0
;;

(* Status after a turn (§4.3 / §4.9):
   - stamina at 0 in a Strained/Critical pose (or with the grace turn
     disabled) -> Fallen;
   - stamina at 0 while Stable -> "pumped out": one grace chance — rest if a
     hand is already on a Rest hold, otherwise the next attempt is desperate;
   - win via [won]. *)
let compute_status (gs : game_state) =
  let stability = Balance.stability gs.wall gs.player.limbs ~torso:gs.player.torso in
  (* §4.7 fall conditions checked here because hold BREAKAGE can invalidate a
     pose the gate never approved: fewer than 2 limbs, or Falling stability
     (which also covers the horizontal-support failure). *)
  if List.length (Player.attached gs.player.limbs) < 2
  then Fallen "too few limbs left on the wall"
  else if [%equal: stability] stability Falling
  then Fallen "the pose came apart"
  else if gs.player.stamina <= 0
  then (
    match stability with
    | Stable when Config.exhaustion_grace_turn -> gs.status
    | Stable -> Fallen "exhausted: no strength left"
    | Strained | Critical | Falling ->
      Fallen
        (sprintf
           !"exhausted in a %{sexp:stability} pose"
           stability))
  else if won ~wall:gs.wall gs.player
  then Won
  else gs.status
;;

(* A fall sends the climber straight back to the start pose (fresh session
   state); the pre-fall state goes onto the history so undo can replay the
   mistake. *)
let fall t ~(pre : game_state) reason =
  let fresh =
    { pre with player = t.start; broken_holds = pre.broken_holds; status = Playing }
  in
  `Fell ({ t with current = fresh; history = pre :: t.history }, reason)
;;

let move t limb ~hold_id =
  let gs = t.current in
  match Wall.find gs.wall hold_id with
  | None -> `Rejected Hold_broken
  | Some hold ->
    (match
       Movement.preview_move ~wall:gs.wall ~broken:gs.broken_holds gs.player limb hold
     with
     | Error Would_fall ->
       fall t ~pre:gs (sprintf "you lost your balance lunging for hold %d" hold_id)
     | Error Limb_stranded ->
       fall
         t
         ~pre:gs
         (sprintf "reaching hold %d yanked another limb off the wall" hold_id)
     | Error Out_of_reach ->
       fall t ~pre:gs (sprintf "hold %d was out of reach - you lunged and missed" hold_id)
     | Error Span_violation ->
       fall t ~pre:gs (sprintf "you overstretched going for hold %d and came off" hold_id)
     | Error reason -> `Rejected reason
     | Ok (player, cost) ->
       if cost.total > gs.player.stamina
       then
         (* The desperate move: physically possible, but the tank is empty —
            the grip gives out mid-move. *)
         fall
           t
           ~pre:gs
           (sprintf
              "grip gave out reaching hold %d (needed %d stamina, had %d)"
              hold_id
              cost.total
              gs.player.stamina)
       else (
         let next, broke = tick_crumbling { gs with player } in
         match compute_status next with
         | Fallen reason ->
           let reason =
             if List.is_empty broke
             then reason
             else
               sprintf
                 "hold %s crumbled under you"
                 (String.concat ~sep:", " (List.map broke ~f:Int.to_string))
           in
           fall t ~pre:gs reason
         | (Playing | Won) as status ->
           `Moved ({ t with current = { next with status }; history = gs :: t.history }, cost)))
;;

let chalk t limb =
  let gs = t.current in
  match Movement.attempt_chalk ~wall:gs.wall ~broken:gs.broken_holds gs.player limb with
  | Error reason -> `Rejected reason
  | Ok player ->
    let next, broke = tick_crumbling { gs with player } in
    (match compute_status next with
     | Fallen reason ->
       let reason =
         if List.is_empty broke then reason else sprintf "%s (a hold crumbled)" reason
       in
       fall t ~pre:gs reason
     | (Playing | Won) as status ->
       `Chalked { t with current = { next with status }; history = gs :: t.history })
;;

let rest t =
  let gs = t.current in
  match Movement.attempt_rest ~wall:gs.wall ~broken:gs.broken_holds gs.player with
  | Error reason -> `Rejected reason
  | Ok player ->
    (* resting on a crumbling hold still wears it *)
    let next, broke = tick_crumbling { gs with player } in
    (match compute_status next with
     | Fallen reason ->
       let reason =
         if List.is_empty broke then reason else sprintf "%s (a hold crumbled)" reason
       in
       fall t ~pre:gs reason
     | (Playing | Won) as status ->
       `Rested { t with current = { next with status }; history = gs :: t.history })
;;

let undo t =
  match t.history with
  | [] -> None
  | previous :: history -> Some { t with current = previous; history }
;;

let reset t = create ~wall:t.current.wall ~start:t.start

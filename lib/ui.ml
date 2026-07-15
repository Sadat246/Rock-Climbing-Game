open! Core
open Types

type t =
  { limb : limb
  ; candidates : int list
  ; index : int
  ; message : string
  }

let select_limb (gs : game_state) limb =
  let current = Player.limb_hold_id gs.player.limbs limb in
  let candidates =
    Array.to_list gs.wall.holds
    |> List.filter_map ~f:(fun hold ->
      if [%equal: int option] current (Some hold.id)
      then None
      else (
        match
          Movement.attempt_move ~wall:gs.wall ~broken:gs.broken_holds gs.player limb hold
        with
        | Ok _ -> Some hold.id
        | Error _ -> None))
    |> List.sort ~compare:Int.compare
  in
  let message =
    if List.is_empty candidates
    then sprintf !"%{sexp:limb} has no reachable holds" limb
    else sprintf !"%{sexp:limb}: %d reachable holds" limb (List.length candidates)
  in
  { limb; candidates; index = 0; message }
;;

let init gs =
  { (select_limb gs Left_hand) with
    message = "1-4 limb | n/p cycle | m move | u undo | q quit"
  }
;;

let cycle t step =
  match List.length t.candidates with
  | 0 -> t
  | n -> { t with index = (t.index + step + n) mod n }
;;

let next t = cycle t 1
let prev t = cycle t (-1)
let target t = List.nth t.candidates t.index
let with_message t message = { t with message }

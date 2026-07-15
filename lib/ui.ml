open! Core
open Types

type t =
  { limb : limb
  ; candidates : int list
  ; desperate : int list
  ; index : int
  ; message : string
  }

let select_limb (gs : game_state) limb =
  let current = Player.limb_hold_id gs.player.limbs limb in
  let classified =
    Array.to_list gs.wall.holds
    |> List.filter_map ~f:(fun hold ->
      if [%equal: int option] current (Some hold.id)
      then None
      else (
        match
          Movement.preview_move ~wall:gs.wall ~broken:gs.broken_holds gs.player limb hold
        with
        | Error _ -> None
        | Ok (_, cost) -> Some (hold.id, cost.total > gs.player.stamina)))
    |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
  in
  let candidates = List.map classified ~f:fst in
  let desperate = List.filter_map classified ~f:(fun (id, d) -> Option.some_if d id) in
  let message =
    match List.length candidates, List.length desperate with
    | 0, _ -> sprintf !"%{sexp:limb} has no reachable holds" limb
    | n, 0 -> sprintf !"%{sexp:limb}: %d reachable holds" limb n
    | n, d -> sprintf !"%{sexp:limb}: %d reachable holds (%d would cost more than you have!)" limb n d
  in
  { limb; candidates; desperate; index = 0; message }
;;

let init gs =
  { (select_limb gs Left_hand) with
    message = "1-4 limb | n/p cycle | m move | r rest | u undo | q quit"
  }
;;

let cycle t step =
  match List.length t.candidates with
  | 0 -> t
  | n -> { t with index = (t.index + step + n) mod n }
;;

let next t = cycle t 1
let prev t = cycle t (-1)

let focus t hold_id =
  match List.findi t.candidates ~f:(fun _ id -> id = hold_id) with
  | Some (index, _) -> { t with index }
  | None -> t
;;

let target t = List.nth t.candidates t.index
let is_desperate t hold_id = List.mem t.desperate hold_id ~equal:Int.equal
let with_message t message = { t with message }
;;

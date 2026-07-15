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
         || Set.mem gs.broken_holds hold.id
      then None
      else Some hold.id)
    |> List.sort ~compare:Int.compare
  in
  { limb; candidates; index = 0; message = sprintf !"%{sexp:limb} selected" limb }
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
let with_message t message = { t with message }

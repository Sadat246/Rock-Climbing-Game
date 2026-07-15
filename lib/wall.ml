open! Core
open Types

let find (wall : wall) id = Array.find wall.holds ~f:(fun h -> h.id = id)

let position_exn wall id =
  match find wall id with
  | Some h -> h.position
  | None -> raise_s [%message "Wall.position_exn: no such hold" (id : int)]
;;

(* Hold coordinates below are wall CONTENT (level data), not tuning constants,
   so they live here rather than in config.ml. Two columns at x = 40/80;
   footholds every 60 units starting at y = 30, jugs every 60 starting at
   y = 60, finish pair at y = 300. Rung spacing is comfortably inside
   hand_reach/foot_reach so the ladder stays a canary, not a challenge. *)
let test_wall_ladder =
  let mk id x y kind = { id; position = { x; y }; kind; durability = None } in
  let columns = [ 40.; 80. ] in
  let rows ~kind ~first_id ~ys =
    List.concat_mapi ys ~f:(fun row y ->
      List.mapi columns ~f:(fun col x -> mk (first_id + (2 * row) + col) x y kind))
  in
  let footholds = rows ~kind:Foothold ~first_id:0 ~ys:[ 30.; 90.; 150.; 210.; 270. ] in
  let jugs = rows ~kind:Jug ~first_id:10 ~ys:[ 60.; 120.; 180.; 240. ] in
  let finishes = rows ~kind:Finish ~first_id:18 ~ys:[ 300. ] in
  { holds = Array.of_list (footholds @ jugs @ finishes)
  ; width = 120
  ; height = 320
  ; finish_y = 300.
  }
;;

let ladder_start =
  let limbs =
    { left_hand = Some 10 (* jug (40, 60) *)
    ; right_hand = Some 11 (* jug (80, 60) *)
    ; left_foot = Some 0 (* foothold (40, 30) *)
    ; right_foot = Some 1 (* foothold (80, 30) *)
    }
  in
  let torso =
    Geometry.average
      (List.map (Player.attached limbs) ~f:(fun (_, id) ->
         position_exn test_wall_ladder id))
  in
  { limbs
  ; torso
  ; stamina = Config.starting_stamina
  ; chalk =
      { remaining = Config.starting_chalk; left_hand_chalk = 0; right_hand_chalk = 0 }
  ; turn = 0
  }
;;

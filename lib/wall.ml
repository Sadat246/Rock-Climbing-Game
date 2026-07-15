open! Core
open Types

let find (wall : wall) id = Array.find wall.holds ~f:(fun h -> h.id = id)

let position_exn wall id =
  match find wall id with
  | Some h -> h.position
  | None -> raise_s [%message "Wall.position_exn: no such hold" (id : int)]
;;

(* Hold coordinates below are wall CONTENT (level data), not tuning constants,
   so they live here rather than in config.ml. *)

let mk id x y kind = { id; position = { x; y }; kind; durability = None }

let two_columns ~kind ~first_id ~xs ~ys =
  List.concat_mapi ys ~f:(fun row y ->
    List.mapi xs ~f:(fun col x -> mk (first_id + (List.length xs * row) + col) x y kind))
;;

let start_pose wall limbs =
  let torso =
    Geometry.average
      (List.map (Player.attached limbs) ~f:(fun (_, id) -> position_exn wall id))
  in
  { limbs
  ; torso
  ; stamina = Config.starting_stamina
  ; chalk =
      { remaining = Config.starting_chalk; left_hand_chalk = 0; right_hand_chalk = 0 }
  ; turn = 0
  }
;;

(* Two columns at x = 40/80: footholds at y = 30 (ids 0,1), jugs every 30
   units from y = 60 to 270 (row r has ids 2r, 2r+1), finish pair at y = 300
   (ids 18,19). Feet climb on the vacated jugs. 30-unit rungs keep every step
   comfortably inside the torso-shift model's limits — under it the torso
   trails each move, so 60-unit rungs would strand the feet behind
   max_foot_above_torso (see TUNING_LOG 2026-07-15 Phase 2). *)
let test_wall_ladder =
  let xs = [ 40.; 80. ] in
  let footholds = two_columns ~kind:Foothold ~first_id:0 ~xs ~ys:[ 30. ] in
  let jugs =
    two_columns
      ~kind:Jug
      ~first_id:2
      ~xs
      ~ys:[ 60.; 90.; 120.; 150.; 180.; 210.; 240.; 270. ]
  in
  let finishes = two_columns ~kind:Finish ~first_id:18 ~xs ~ys:[ 300. ] in
  { holds = Array.of_list (footholds @ jugs @ finishes)
  ; width = 120
  ; height = 320
  ; finish_y = 300.
  }
;;

let ladder_start =
  start_pose
    test_wall_ladder
    { left_hand = Some 2 (* jug (40, 60) *)
    ; right_hand = Some 3 (* jug (80, 60) *)
    ; left_foot = Some 0 (* foothold (40, 30) *)
    ; right_foot = Some 1 (* foothold (80, 30) *)
    }
;;

(* §6.2 overhang_lean: a foot desert. Footholds exist only at the bottom;
   a column of jugs climbs away above them. Hands climb until the torso has
   been dragged high above the stranded feet — reaching back down to a low
   jug then leaves the torso hanging far off the support center (Critical) —
   and the next upward move is rejected because the shifted torso would
   strand a foot beyond its reach (the §6.4 no-teleport check). *)
let test_wall_overhang =
  let footholds = [ mk 0 40. 30. Foothold; mk 1 60. 30. Foothold ] in
  let jugs =
    [ mk 2 40. 90. Jug
    ; mk 3 60. 90. Jug
    ; mk 4 40. 120. Jug
    ; mk 5 60. 120. Jug
    ; mk 6 40. 150. Jug
    ; mk 7 60. 150. Jug
    ; mk 8 50. 130. Jug
    ; mk 9 40. 180. Jug
    ]
  in
  { holds = Array.of_list (footholds @ jugs)
  ; width = 100
  ; height = 200
  ; finish_y = 200. (* no finish holds: this wall is a balance scenario *)
  }
;;

let overhang_start =
  start_pose
    test_wall_overhang
    { left_hand = Some 2 (* jug (40, 90) *)
    ; right_hand = Some 3 (* jug (60, 90) *)
    ; left_foot = Some 0 (* foothold (40, 30) *)
    ; right_foot = Some 1 (* foothold (60, 30) *)
    }
;;

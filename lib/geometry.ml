open! Core
open Types

let distance a b = Float.hypot (a.x -. b.x) (a.y -. b.y)

let average points =
  match points with
  | [] -> failwith "Geometry.average: empty point list"
  | _ ->
    let n = Float.of_int (List.length points) in
    { x = List.sum (module Float) points ~f:(fun p -> p.x) /. n
    ; y = List.sum (module Float) points ~f:(fun p -> p.y) /. n
    }
;;

let max_reach = function
  | Left_hand | Right_hand -> Config.hand_reach
  | Left_foot | Right_foot -> Config.foot_reach
;;

let shift_toward p ~target ~factor =
  { x = p.x +. (factor *. (target.x -. p.x)); y = p.y +. (factor *. (target.y -. p.y)) }
;;

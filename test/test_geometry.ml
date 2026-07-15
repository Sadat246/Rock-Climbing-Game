open! Core
open Climb

let p x y = { Types.x; y }

let%expect_test "distance" =
  printf "3-4-5: %.3f\n" (Geometry.distance (p 0. 0.) (p 3. 4.));
  printf "zero:  %.3f\n" (Geometry.distance (p 1. 1.) (p 1. 1.));
  printf "neg:   %.3f\n" (Geometry.distance (p (-3.) 0.) (p 0. 4.));
  [%expect {|
    3-4-5: 5.000
    zero:  0.000
    neg:   5.000
    |}]
;;

let%expect_test "average" =
  print_s [%sexp (Geometry.average [ p 2. 6. ] : Types.point)];
  print_s [%sexp (Geometry.average [ p 0. 0.; p 10. 20. ] : Types.point)];
  print_s
    [%sexp
      (Geometry.average [ p 0. 0.; p 100. 0.; p 0. 100.; p 100. 100. ] : Types.point)];
  [%expect {|
    ((x 2) (y 6))
    ((x 5) (y 10))
    ((x 50) (y 50))
    |}]
;;

let%expect_test "average of nothing raises" =
  Expect_test_helpers_core.require_does_raise (fun () -> Geometry.average []);
  [%expect {| (Failure "Geometry.average: empty point list") |}]
;;

let%expect_test "reach asymmetry preserved (guards config edits, §6.4)" =
  List.iter Types.all_of_limb ~f:(fun limb ->
    printf !"%{sexp:Types.limb} reach %.0f\n" limb (Geometry.max_reach limb));
  Expect_test_helpers_core.require
    (Float.( > ) (Geometry.max_reach Types.Left_hand) (Geometry.max_reach Types.Left_foot));
  [%expect {|
    Left_hand reach 110
    Right_hand reach 110
    Left_foot reach 90
    Right_foot reach 90
    |}]
;;

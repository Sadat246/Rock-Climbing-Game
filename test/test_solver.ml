open! Core
open Climb
open Climb.Types

(* Solver behaviors on the small/fast cases. The full-ladder solve (with its
   band checks and the keystone replay) lives in test_tuning.ml — it costs a
   few seconds and we only want to pay it once. *)

let%expect_test "overhang: no finish holds, search space exhausts cleanly" =
  (match Solver.solve ~blocked:(Set.empty (module Int)) ~wall:Wall.test_wall_overhang ~start:Wall.overhang_start with
   | No_route { states_expanded } -> printf "no route, %d states expanded\n" states_expanded
   | Solution _ -> print_endline "unexpectedly solved"
   | Search_limit _ -> print_endline "unexpectedly hit the cap");
  [%expect {| no route, 7630 states expanded |}]
;;

let%expect_test "determinism: same wall, same outcome, same route" =
  let solve () = Solver.solve ~blocked:(Set.empty (module Int)) ~wall:Wall.test_wall_overhang ~start:Wall.overhang_start in
  let a = solve () in
  let b = solve () in
  printf
    "identical: %b\n"
    (String.equal
       (Sexp.to_string [%sexp (a : Solver.outcome)])
       (Sexp.to_string [%sexp (b : Solver.outcome)]));
  [%expect {| identical: true |}]
;;

(* Starting with a fifth of the tank, the solver discovers it must dash to
   the y=120 Rest row before running dry, shake out, and continue — a
   rest-managed route with a knife-edge minimum of 3 stamina. This is the
   solver doing real route-planning, not just pathfinding. *)
let%expect_test "low stamina: the solver rest-manages its way up" =
  let start = { Wall.ladder_start with stamina = 20 } in
  (match Solver.solve ~blocked:(Set.empty (module Int)) ~wall:Wall.test_wall_ladder ~start with
   | No_route { states_expanded } -> printf "no route, %d states expanded\n" states_expanded
   | Solution { actions; metrics } ->
     let rests = List.count actions ~f:(function Solver.Rest -> true | Move _ | Chalk _ -> false) in
     printf !"solved with %d rests: %{sexp:Solver.metrics}\n" rests metrics
   | Search_limit _ -> print_endline "unexpectedly hit the cap");
  [%expect {|
    solved with 5 rests: ((optimal_cost 99) (optimal_moves 18) (chalk_required 0)
     (min_stamina_remaining 8) (states_expanded 254066) (max_queue_size 84437)
     (critical_balance_turns 0))
    |}]
;;

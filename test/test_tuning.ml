open! Core
open Climb
open Climb.Types

(* §6.3/§6.4: the solver as playtester, plus the KEYSTONE invariant — any
   route the solver returns must replay through the real Game to Won. Target
   bands are asserted with [require]; the printed metrics make drift visible
   in the diff when a constant changes. Band changes need a TUNING_LOG
   entry (§6.6). *)

let replay_through_game ~wall ~start (actions : Solver.action list) =
  let final =
    List.fold actions ~init:(Game.create ~wall ~start) ~f:(fun game action ->
      match action with
      | Solver.Rest ->
        (match Game.rest game with
         | `Rested game -> game
         | `Fell (_, reason) -> raise_s [%message "replay rest fell" (reason : string)]
         | `Rejected r -> raise_s [%message "replay rest rejected" (r : reject_reason)])
      | Solver.Chalk limb ->
        (match Game.chalk game limb with
         | `Chalked game -> game
         | `Fell (_, reason) -> raise_s [%message "replay chalk fell" (reason : string)]
         | `Rejected r -> raise_s [%message "replay chalk rejected" (r : reject_reason)])
      | Solver.Move (limb, hold_id) ->
        (match Game.move game limb ~hold_id with
         | `Moved (game, _) -> game
         | `Fell (_, reason) -> raise_s [%message "replay fell" (reason : string)]
         | `Rejected r ->
           raise_s
             [%message "replay rejected" (r : reject_reason) (limb : limb) (hold_id : int)]))
  in
  (Game.current_state final).status
;;

let%expect_test "ladder canary via the solver: solves, replays to Won, in-band" =
  (match Solver.solve ~wall:Wall.test_wall_ladder ~start:Wall.ladder_start with
   | No_route _ | Search_limit _ ->
     print_endline "LADDER DID NOT SOLVE - a constant crossed a hard floor (§6.3)"
   | Solution { actions; metrics } ->
     (* keystone: solver and game must agree *)
     let status = replay_through_game ~wall:Wall.test_wall_ladder ~start:Wall.ladder_start actions in
     printf !"replay status: %{sexp:game_status}\n" status;
     Expect_test_helpers_core.require ([%equal: game_status] status Won);
     printf !"%{sexp:Solver.metrics}\n" metrics;
     (* target bands for the canary — it should be EASY but not free *)
     let check name ok = Expect_test_helpers_core.require ok ~if_false_then_print_s:(lazy (Sexp.Atom name)) in
     check "solvable in few moves" (metrics.optimal_moves <= 20);
     check "wins with margin" (metrics.min_stamina_remaining >= 10);
     check "no chalk needed" (metrics.chalk_required = 0);
     check "no critical poses on the easy wall" (metrics.critical_balance_turns = 0);
     check "state space in check (§6.3 leak canary)" (metrics.states_expanded < 200_000));
  [%expect {|
    replay status: Won
    ((optimal_cost 93) (optimal_moves 13) (chalk_required 0)
     (min_stamina_remaining 20) (states_expanded 137525) (max_queue_size 65130)
     (critical_balance_turns 0))
    |}]
;;

let%expect_test "sloper_gate: solver finds the chalk-dependent route, in-band" =
  (match Solver.solve ~wall:Wall.test_wall_sloper_gate ~start:Wall.sloper_gate_start with
   | No_route _ | Search_limit _ -> print_endline "SLOPER GATE DID NOT SOLVE"
   | Solution { actions; metrics } ->
     let status =
       replay_through_game
         ~wall:Wall.test_wall_sloper_gate
         ~start:Wall.sloper_gate_start
         actions
     in
     printf !"replay status: %{sexp:game_status}\n" status;
     Expect_test_helpers_core.require ([%equal: game_status] status Won);
     printf !"%{sexp:Solver.metrics}\n" metrics;
     let check name ok =
       Expect_test_helpers_core.require ok ~if_false_then_print_s:(lazy (Sexp.Atom name))
     in
     (* §6.3 target bands for the gate wall *)
     check "chalk is forced (>= 2 chalk actions)" (metrics.chalk_required >= 2);
     check "low stamina margin (hard wall)" (metrics.min_stamina_remaining <= 30)
   );
  [%expect {|
    replay status: Won
    ((optimal_cost 132) (optimal_moves 13) (chalk_required 2)
     (min_stamina_remaining 1) (states_expanded 110533) (max_queue_size 32546)
     (critical_balance_turns 0))
    |}]
;;

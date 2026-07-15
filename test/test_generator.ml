open! Core
open Climb
open Climb.Types

(* Phase 6 invariants. Short walls (height 200) keep solver time down; the
   full-size acceptance sweep is a CLI tuning run (`sweep`), not a unit test
   (documented deviation from §5 Phase 6's "100 seeds" — see TUNING_LOG). *)

let%expect_test "determinism: same seed, same wall, hold for hold" =
  let gen () = Generator.generate ~height:200. ~seed:1 ~difficulty:Medium () in
  (match gen (), gen () with
   | Ok a, Ok b ->
     printf
       "identical: %b (%d holds)\n"
       (String.equal
          (Sexp.to_string [%sexp (a : Generator.generated)])
          (Sexp.to_string [%sexp (b : Generator.generated)]))
       (Array.length a.wall.holds)
   | Error e, _ | _, Error e -> printf !"generation failed: %{Error#hum}\n" e);
  [%expect {| identical: true (24 holds) |}]
;;

(* The §4.13 promise: generated walls are solvable BY CONSTRUCTION, and the
   solver's route replays through the real Game to Won (the §6.4 keystone,
   extended to generated content). *)
let%expect_test "generated walls: solvable by construction, keystone replay" =
  List.iter [ 1; 2; 3 ] ~f:(fun seed ->
    match Generator.generate ~height:200. ~seed ~difficulty:Medium () with
    | Error e -> printf !"seed %d: generation failed (%{Error#hum})\n" seed e
    | Ok g ->
      (match Solver.solve ~blocked:(Set.empty (module Int)) ~wall:g.wall ~start:g.start with
       | No_route _ ->
         printf "seed %d: UNSOLVABLE - §4.13 violated, this is a bug\n" seed
       | Search_limit _ -> printf "seed %d: solver capped\n" seed
       | Solution { actions; metrics } ->
         let status =
           List.fold actions ~init:(Game.create ~wall:g.wall ~start:g.start) ~f:(fun game action ->
             match action with
             | Solver.Rest ->
               (match Game.rest game with
                | `Rested game -> game
                | `Fell _ | `Rejected _ -> failwith "replay rest failed")
             | Solver.Chalk limb ->
               (match Game.chalk game limb with
                | `Chalked game -> game
                | `Fell _ | `Rejected _ -> failwith "replay chalk failed")
             | Solver.Move (limb, hold_id) ->
               (match Game.move game limb ~hold_id with
                | `Moved (game, _) -> game
                | `Fell _ | `Rejected _ -> failwith "replay move failed"))
           |> Game.current_state
           |> fun gs -> gs.status
         in
         printf
           !"seed %d: solved, %d moves, margin %d, replay %{sexp:game_status}\n"
           seed
           metrics.optimal_moves
           metrics.min_stamina_remaining
           status));
  [%expect {|
    seed 1: solved, 3 moves, margin 68, replay Won
    seed 2: solved, 4 moves, margin 68, replay Won
    seed 3: solved, 4 moves, margin 66, replay Won
    |}]
;;

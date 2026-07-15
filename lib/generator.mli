open! Core

(** Guaranteed-route wall generation (§4.13). The route is built FIRST, by
    simulating a climber whose every action goes through the gate
    (attempt_move / attempt_rest / attempt_chalk) — so an accepted wall is
    solvable BY CONSTRUCTION, with rest and chalk stops inserted where the
    simulated climber actually needed them. Decoy holds are sprinkled
    afterwards; the solver-based [evaluate] rejects walls that came out too
    easy (or, which would be a bug, unsolvable).

    Fully deterministic: all randomness flows from the seed. *)

type difficulty =
  | Easy
  | Medium
  | Hard
[@@deriving sexp_of, compare, equal]

val difficulty_of_string : string -> difficulty option

type generated =
  { wall : Types.wall
  ; start : Types.player_state
  ; route_holds : int list (* ids the construction climb used, in order *)
  ; decoy_holds : int list
  ; difficulty : difficulty
  ; seed : int
  }
[@@deriving sexp_of]

(** [height] overrides [Config.gen_wall_height] (tests use short walls to
    keep solver time down). Fails if the seed paints itself into a corner
    (rare; try the next seed). *)
val generate : ?height:float -> seed:int -> difficulty:difficulty -> unit -> generated Or_error.t

type verdict =
  | Accepted of
      { metrics : Solver.metrics
      ; families : int
      ; actions : Solver.action list
      }
  | Rejected of string
[@@deriving sexp_of]

(** Solve + §4.13 step 6: reject unsolvable (a generator bug) or out-of-band
    walls; count route families (§4.14) by blocking the optimal route's hand
    holds and re-solving, up to the family cap. *)
val evaluate : generated -> verdict

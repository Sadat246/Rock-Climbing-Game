open! Core

(** Look up a hold on the wall by id. *)
val find : Types.wall -> int -> Types.hold option

(** Position of the hold with the given id. Raises if absent — callers use it
    only for ids already validated or stored in an accepted state. *)
val position_exn : Types.wall -> int -> Types.point

(** Trivially climbable two-column jug ladder (the permanent canary wall —
    CLAUDE.md §6.2: it must ALWAYS be climbable/solvable). *)
val test_wall_ladder : Types.wall

(** Valid four-limb starting pose at the bottom of [test_wall_ladder]. *)
val ladder_start : Types.player_state

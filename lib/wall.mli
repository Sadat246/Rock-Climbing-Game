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

(** §6.2 balance scenario (foot desert): footholds only at the bottom, jugs
    climbing away above. Climbing hands-high then reaching back down leaves
    the pose Critical; the next upward move is rejected. No finish holds. *)
val test_wall_overhang : Types.wall

val overhang_start : Types.player_state

(** §6.2 sloper_gate: the only route up crosses two slopers — unchalked they
    drain the tank, chalked they're crossable. Chalk_refill pocket below. *)
val test_wall_sloper_gate : Types.wall

val sloper_gate_start : Types.player_state

(** §6.2 crumble_trap: a crumbling jug (durability 2). Scenario wall. *)
val test_wall_crumble_trap : Types.wall

val crumble_trap_start : Types.player_state

(** Registry of hand-built walls with their starting poses, for the CLI and
    the tuning loop. *)
val all : (string * (Types.wall * Types.player_state)) list

val find_by_name : string -> (Types.wall * Types.player_state) option

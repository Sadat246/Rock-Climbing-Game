open! Core

(** A play session: current state plus undo history (trivial thanks to
    immutable states — CLAUDE.md rule 6). Turn loop, win/loss, status. *)
type t =
  { current : Types.game_state
  ; history : Types.game_state list
  }

val create : wall:Types.wall -> start:Types.player_state -> t
val current_state : t -> Types.game_state

(** Move a limb to the hold with [hold_id], via the single gate.
    Rejections leave the session unchanged. *)
val move
  :  t
  -> Types.limb
  -> hold_id:int
  -> [ `Moved of t | `Rejected of Types.reject_reason ]

(** Step back one accepted move. [None] at the start of the session. *)
val undo : t -> t option

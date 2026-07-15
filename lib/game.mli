open! Core

(** A play session: current state plus undo history (trivial thanks to
    immutable states — CLAUDE.md rule 6). Turn loop, win/loss, status. *)
type t =
  { current : Types.game_state
  ; history : Types.game_state list
  ; start : Types.player_state (* where falls send you back to *)
  }

val create : wall:Types.wall -> start:Types.player_state -> t
val current_state : t -> Types.game_state

(** Move a limb to the hold with [hold_id], via the single gate.

    [`Moved] carries the move's cost for HUD messages. [`Fell] is a committed
    move the climber could not afford — the grip gives out and the status
    becomes [Fallen] (restart or undo from there). Impossible moves are
    [`Rejected] and leave the session unchanged. *)
val move
  :  t
  -> Types.limb
  -> hold_id:int
  -> [ `Moved of t * Movement.cost | `Fell of t | `Rejected of Types.reject_reason ]

(** Rest (§4.3): hand on a Rest hold + a foot attached + Stable. *)
val rest : t -> [ `Rested of t | `Rejected of Types.reject_reason ]

(** Step back one accepted move (works from a Fallen state too). *)
val undo : t -> t option

(** Back to the starting pose with fresh stamina — the fall consequence. *)
val reset : t -> t

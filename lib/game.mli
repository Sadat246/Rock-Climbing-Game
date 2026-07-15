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

    [`Moved] carries the move's cost for HUD messages.

    [`Fell reason]: the climber went for it and came off the wall — an
    out-of-reach lunge, an overstretch ([Span_violation]), an off-balance
    position ([Would_fall]), a stranded limb ([Limb_stranded]), a move the
    tank couldn't cover, or exhaustion in a bad pose. The returned session
    is ALREADY back at the start pose (owner decision: falls return you to
    the start, immediately); the pre-fall state is pushed onto the history
    so undo can still inspect what went wrong.

    [`Rejected] is only for what can't even be attempted (broken/missing
    hold, occupied hold, wrong limb type) and leaves the session unchanged. *)
val move
  :  t
  -> Types.limb
  -> hold_id:int
  -> [ `Moved of t * Movement.cost | `Fell of t * string | `Rejected of Types.reject_reason ]

(** Rest (§4.3): hand on a Rest hold + a foot attached + Stable. *)
val rest : t -> [ `Rested of t | `Rejected of Types.reject_reason ]

(** Step back one accepted move (works from a Fallen state too). *)
val undo : t -> t option

(** Back to the starting pose with fresh stamina — the fall consequence. *)
val reset : t -> t

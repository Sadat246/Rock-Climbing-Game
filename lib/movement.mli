open! Core

(** THE single validation gate (CLAUDE.md §4.11, rule 2). Every state
    transition — player input, solver edge expansion, generator
    route-building — must go through here. Never duplicate validation logic.

    Phase 0 checks, in order: hold exists on wall & not broken → not illegally
    occupied → limb-compatible → within reach of the torso (incl. §4.5
    vertical limits). Spans, balance, stamina, and chalk arrive in Phases 2–5.

    On [Ok], the returned state has the limb reattached, the torso recomputed
    (Phase 0: average of attached limbs), and the turn incremented. Pure:
    the input state is never mutated. *)
val attempt_move
  :  wall:Types.wall
  -> broken:Set.M(Int).t
  -> Types.player_state
  -> Types.limb
  -> Types.hold
  -> (Types.player_state, Types.reject_reason) result

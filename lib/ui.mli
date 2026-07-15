open! Core

(** Selection UI state for interactive play — which limb is active, which
    hold the cursor points at. Pure; shared by the Graphics window and the
    ASCII terminal loop so the two frontends can never disagree.

    Owner decision 2026-07-15: the UI reveals NOTHING about reachability —
    finding where a limb can go is the puzzle. The cursor ranges over every
    hold on the wall; attempting a hold is how you find out. *)
type t =
  { limb : Types.limb
  ; candidates : int list
    (* every hold on the wall except the one the limb is on, ascending *)
  ; index : int (* position in [candidates]; meaningless when empty *)
  ; message : string (* last feedback line for the HUD *)
  }

(** Select a limb; the cursor covers all holds (no reachability filtering). *)
val select_limb : Types.game_state -> Types.limb -> t

(** [select_limb] on [Left_hand], with a welcome message. *)
val init : Types.game_state -> t

val next : t -> t
val prev : t -> t

(** Point the cursor at a specific hold id (no-op if it isn't a candidate). *)
val focus : t -> int -> t

(** Hold the cursor points at, if any. *)
val target : t -> int option

val with_message : t -> string -> t

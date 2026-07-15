open! Core

(** Selection UI state for interactive play — which limb is active, which
    reachable hold is highlighted. Pure; shared by the Graphics window and
    the ASCII terminal loop so the two frontends can never disagree. *)
type t =
  { limb : Types.limb
  ; candidates : int list (* hold ids the limb can legally move to, ascending *)
  ; index : int (* position in [candidates]; meaningless when empty *)
  ; message : string (* last feedback line for the HUD *)
  }

(** Select a limb and compute its legal destinations by running every hold on
    the wall through Movement.attempt_move (the single gate decides
    reachability — never a second check). The limb's current hold is
    excluded. *)
val select_limb : Types.game_state -> Types.limb -> t

(** [select_limb] on [Left_hand], with a welcome message. *)
val init : Types.game_state -> t

val next : t -> t
val prev : t -> t

(** Highlighted destination hold, if any candidate exists. *)
val target : t -> int option

val with_message : t -> string -> t

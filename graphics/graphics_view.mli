open! Core

(** OCaml-Graphics (X11 window) renderer. A thin consumer of [game_state] —
    no game logic, no state of its own. The engine library never links this. *)

(** Open the window, sized from the wall dimensions. *)
val init : Climb.Types.wall -> unit

(** Redraw the whole scene: holds as dots, climber as a stick figure. *)
val draw : Climb.Types.game_state -> unit

(** Block until any key is pressed in the window. *)
val wait_for_key : unit -> unit

val close : unit -> unit

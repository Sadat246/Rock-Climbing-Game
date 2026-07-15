open! Core

(** OCaml-Graphics (X11 window) renderer. A thin consumer of [game_state] —
    no game logic, no state of its own. The engine library never links this. *)

(** Open the window, sized from the wall dimensions. Returns [Error message]
    when no X display is reachable (e.g. SSH without X forwarding) so callers
    can fall back to ASCII instead of crashing. *)
val init : Climb.Types.wall -> (unit, string) result

(** Redraw the whole scene: holds as dots, climber as a stick figure. *)
val draw : Climb.Types.game_state -> unit

(** [draw] plus interactive overlays: selected limb in green, target hold
    ringed in green, two HUD text lines at the top. *)
val draw_with_ui : Climb.Types.game_state -> Climb.Ui.t -> unit

(** Block until a key is pressed in the window; return it. *)
val read_key : unit -> char

(** Block until any key is pressed in the window. *)
val wait_for_key : unit -> unit

val close : unit -> unit

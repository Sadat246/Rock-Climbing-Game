open! Core

(** OCaml-Graphics (X11 window) renderer. A thin consumer of [game_state] —
    no game logic, no state of its own. The engine library never links this. *)

(** Open the window, sized from the wall dimensions. Returns [Error message]
    when no X display is reachable (e.g. SSH without X forwarding) so callers
    can fall back to ASCII instead of crashing. *)
val init : Climb.Types.wall -> (unit, string) result

(** Redraw the whole scene: holds as dots, climber as a stick figure. *)
val draw : Climb.Types.game_state -> unit

(** [draw] plus interactive overlays: reachable holds pale-ringed, selected
    limb in green, target hold bold-ringed, ghost torso (post-move preview,
    gray), and HUD lines incl. current/preview balance tinted by stability. *)
val draw_with_ui : Climb.Types.game_state -> Climb.Ui.t -> unit

(** Tumbling-figure animation from the pre-fall position ([from]) down to
    the reset position ([landed]). Blocks for ~half a second; display only. *)
val animate_fall : from:Climb.Types.game_state -> landed:Climb.Types.game_state -> unit

type event =
  | Key of char
  | Click of int option (* hold id under the pointer, if any *)
  | Hover of int option

(** Block for the next key press, mouse click, or pointer motion. *)
val next_event : Climb.Types.wall -> event

(** Block until a key is pressed in the window; return it. *)
val read_key : unit -> char

(** Block until any key is pressed in the window. *)
val wait_for_key : unit -> unit

val close : unit -> unit

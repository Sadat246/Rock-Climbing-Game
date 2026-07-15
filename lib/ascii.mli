open! Core

(** Pure ASCII snapshot of a game state — the debugging/expect-test instrument
    (CLAUDE.md §6.1), not the game's UI (that's the Graphics window).

    Glyphs: [.] hold, [F] finish hold, [T] torso, [h]/[H] left/right hand,
    [f]/[Q] left/right foot. Limbs draw over the torso; the torso draws over
    holds. Broken holds are not drawn. Last line is a status line. *)
val render : Types.game_state -> string

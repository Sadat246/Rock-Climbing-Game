open! Core

(** Pure ASCII snapshot of a game state — the debugging/expect-test instrument
    (CLAUDE.md §6.1), not the game's UI (that's the Graphics window).

    Hold glyphs (CLAUDE.md Phase 1 table): [J] jug, [c] crimp, [s] sloper,
    [.] foothold, [R] rest, [!] crumbling, [*] chalk refill, [F] finish.
    Body: [T] torso, [t] ghost torso, [h]/[H] left/right hand, [f]/[Q]
    left/right foot, [@] highlighted target. Limbs draw over the torso; the
    torso draws over holds. Broken holds are not drawn. Last line is a
    status line. *)
val render : Types.game_state -> string

(** [render] plus interactive overlays: the highlighted target hold drawn as
    [@] (over everything else), and HUD lines for the selected limb, target,
    reachable holds, and the last feedback message. *)
val render_with_ui : Types.game_state -> Ui.t -> string

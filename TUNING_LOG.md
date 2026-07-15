# TUNING_LOG

Append-only record of tuning sessions and mechanic changes (CLAUDE.md §7).

## 2026-07-15 — Phase 0 bootstrap (no tuning performed)
Hypothesis: n/a — initial implementation.
Change: all constants taken verbatim from CLAUDE.md §8 into `lib/config.ml`.
  Added four rendering-only constants (`ascii_cell` 10., `pixels_per_unit` 2.,
  `window_margin_px` 20, `render_delay_s` 0.4) — display, never gameplay.
Result: ladder canary climbs in 14 moves (scripted), torso rising monotonically
  (60,45) → (60,255); every reject path (`Hold_broken`, `Hold_occupied`,
  `Wrong_limb_for_hold`, `Out_of_reach`) exercised in tests. Phase 0 has no
  stamina/balance/chalk, so no difficulty metrics yet — solver arrives Phase 4.
Verdict: baseline established. Bands: none defined yet.

Note for later phases: `max_foot_above_torso = 15.` is already binding on the
ladder — feet land exactly 15 units above the torso mid-script. Fine for a
canary, but when the real torso model (Phase 2) lands, re-check that ladder
foot moves stay comfortably legal.

## 2026-07-15 — Phase 1 interactive play (no tuning performed)
Hypothesis: n/a — no constants changed.
Change: none in config.ml. Reachability shown to the player is computed by
  running every wall hold through attempt_move (single gate, rule 2).
Result: from the ladder start each hand sees 3 candidates, each foot 3
  (including the y=60 jugs at exactly the +15 foot-above-torso limit — same
  bindingness noted above, now player-visible). Win via Game.compute_status:
  both hands on Finish + ≥1 foot attached; stamina clause joins in Phase 3.
Verdict: baseline unchanged. Bands: none defined yet.

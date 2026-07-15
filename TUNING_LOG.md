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

## 2026-07-15 — Phase 3 stamina/grip: rest rows on the ladder, desperate-move falls
Hypothesis: the §8 cost table would leave the 30-move ladder script winnable
  on the 100 starting stamina.
Change: NO constants changed. The climb costs ~170 (hand move 6 = 3 up + 1
  stable upkeep + 2 jug grip; foot move 5), so this is a CONTENT fix again:
  ladder rows y=120 and y=210 became Rest holds. Canary script: 30 moves + 8
  rests = 38 turns, stamina at the top 38 (> 0, required for the win).
Mechanic decision (owner request, recorded in CLAUDE.md Phase 3 note):
  unaffordable-but-reachable moves are shown (red ring, "YOU WILL FALL"
  preview) and committing to one is a FALL to the start, not a rejection.
  attempt_move still rejects them (Insufficient_stamina) so the solver is
  unaffected. Deterministic and fully telegraphed — no hidden punishment.
Observed numbers worth keeping an eye on (targets get bands in Phase 4):
  - straight-up hand move on jugs: 6; cross-body sideways: 7-8.
  - rest +15/turn means ~3-5 turns of shaking out per stop — feels right.
  - overhang_lean traverse costs 6,6,6,9,9,13 — Strained/Critical upkeep and
    grip multipliers are now the dominant term on bad poses ✓ (the §1 causal
    chain is live: bad balance → pricier grip → stamina drain).
Result: ladder Won at 38 turns / 38 stamina; exhaustion falls verified both
  ways (0-while-Strained falls; 0-while-Stable gets the grace + rest escape);
  desperate falls + reset verified.
Verdict: keep all constants. Bands: define in Phase 4 with the solver.

## 2026-07-15 — Phase 2 torso/spans/balance: ladder densified, gate findings
Hypothesis: the Phase 0/1 ladder (60-unit rungs) would survive the real torso
  model (`torso_shift_factor 0.25`, reach from torso, §4.5 vertical limits).
Change: NO constants changed — all §8 values kept. The hand simulation showed
  60-unit rungs strand the feet: the torso trails each move, so a foot
  stepping +60 exceeds `max_foot_above_torso` (15) long before the torso
  catches up. Fixed as CONTENT, not tuning: ladder jugs now every 30 units
  (feet climb the jugs the hands vacate); canary script is 30 moves, all
  Stable, torso +30/cycle steady state.
Design findings (§6.5.5, formula-level observations):
  1. The §6.4 no-teleport post-shift reach check is the practical stopper for
     over-ambitious routes — it fires before balance reaches Falling in every
     natural sequence tried (a limb gets stranded > its reach first).
  2. Balance-Falling via distance is nearly unreachable from equilibrium
     poses (single move changes d by ≤ ~|torso−limb|/4 ≈ 27). `Would_fall`
     in practice means the horizontal-support check (torso outside
     [support min−15, max+15]) — unit-tested that way.
  3. Critical poses ARE reachable and feel right: climb hands-high off a
     foot desert, then reach back down (overhang_lean scenario hits d=41.1
     Critical on turn 6, next upward move rejected).
  Phase 3 stamina will make Strained/Critical *cost* rather than block; the
  balance multipliers get their teeth there.
Result: ladder canary 30 moves Won, 0 rejections; overhang_lean Critical + 
  terminal Out_of_reach; all §4.6 span constraints exercised in tests.
Verdict: keep all constants. Bands: none defined yet (solver is Phase 4).

## 2026-07-15 — Phase 1 interactive play (no tuning performed)
Hypothesis: n/a — no constants changed.
Change: none in config.ml. Reachability shown to the player is computed by
  running every wall hold through attempt_move (single gate, rule 2).
Result: from the ladder start each hand sees 3 candidates, each foot 3
  (including the y=60 jugs at exactly the +15 foot-above-torso limit — same
  bindingness noted above, now player-visible). Win via Game.compute_status:
  both hands on Finish + ≥1 foot attached; stamina clause joins in Phase 3.
Verdict: baseline unchanged. Bands: none defined yet.

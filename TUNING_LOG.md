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

## 2026-07-15 — Phase 6 generation: sparse-by-construction, sizing experiments
Hypothesis: route-first construction through the gate (§4.13) with small
  steps (hand dy 15-55) would produce puzzly walls.
Reality (empirical loop, all knobs in config.ml):
  1. Small construction steps -> DENSE routes -> the solver leaps past most
     holds: 8-move optima on 260-tall walls, all rejected "too easy". The
     Phase 4 ladder lesson generalizes: THE ROUTE ITSELF must be sparse.
     Fix: construction steps near the climber's real limits (hand dy
     Easy 30-55 / Medium 40-70 / Hard 48-80; feet 20-45), min hold spacing
     14 -> 18, decoys 8-16 -> 6-10 (every hold multiplies solver branching).
  2. Wall sizing: 260 tall caps optima at ~8-9 moves structurally; 340 tall
     lands the band (first accepted wall: 13 moves, margin 17, families 1,
     ~40s solve). gen_wall_width 160 -> 140.
  3. Generation robustness: feet initially couldn't follow (targets above
     torso+15 burned every retry -> "stuck" seeds). Fix: clamp foot targets
     under the vertical limit, hands-lead limb picking, all-limbs rescue
     before bailing. Finish placement made forgiving (wide x, banded y,
     finish_y from what actually landed). Some seeds still fail generation
     (finish pose) — seeds are free, acceptance quality > yield.
  4. §5 Phase 6's "100 seeds" acceptance test is a CLI tuning run
     (`sweep --from N --count K`), not a unit test — full-size solves cost
     tens of seconds each. Unit tests pin determinism + the §4.13/§6.4
     keystone on short (height 200) walls.
Result (sweep, seeds 1-8 medium @ 140x340): 4/8 ACCEPTED — moves 13/14/14/14,
  margins 17/12/9/14, families 1/1/2/2, chalk 0, critical 0; 4 generation
  failures; ZERO unsolvable and zero out-of-band among generated walls (the
  by-construction guarantee held). ~11m40s wall for 8 seeds incl. family
  re-solves. Future knobs: chalk-forced generated walls need a higher
  special rate or sloper clusters (hard difficulty); critical-pose routes
  need overhang-style foot deserts in the generator's vocabulary.

## 2026-07-15 — Phase 5 chalk/crumbling: solver dominance rework, sloper_gate content
Hypothesis: chalk edges would slot into the Phase 4 solver unchanged.
Reality (all changes are solver internals; NO gameplay constants touched):
  1. Chalk edges exploded the chalk-less ladder 43.7k -> 412k states
     (useless chalking multiplies the key space). Fix: skip chalk edges on
     walls with no crimps/slopers — provably optimality-preserving there.
  2. Phase 4's limbs-level dominance was quietly OVER-pruning: on
     sloper_gate it claimed a cost-130 route when a 96 existed, and after a
     wall tweak it claimed "no route" for a solvable wall. Fix: dominance
     only compares states in the same torso REGION (3x the key bucket),
     Pareto over (stamina, bag, lh chalk, rh chalk; g). Ladder: cost 93
     (exact 91), 137k states ~15s. Slower than Phase 4's 4.5s but honest.
     Lesson recorded: aggressive dominance must respect every axis that
     affects reachability, and the canary alone was NOT enough to catch it —
     only the second wall exposed it. More walls = better solver coverage.
  3. sloper_gate content needed 3 iterations: refill pocket at (60,110)
     doubled as a launchpad letting one hand skip the gate (fixed by raising
     the top jugs); 230-high jugs made the wall genuinely impossible
     (solver exhausted 527k states); 220 landed it: cost 132, 13 moves,
     chalk_required 2, min_stamina 1, replay Won. Unchalked scripted attempt
     dies mid-gate (94 -> 26 in three sloper turns, then all rejects). Bands:
     chalk_required >= 2, min_stamina <= 30.
  4. compute_status now enforces §4.7 fall conditions (<2 limbs, Falling)
     because hold breakage creates poses the gate never approved — found by
     the crumble_trap scenario test, invisible to every earlier test.
  5. Emergent mechanic (kept, deliberately): re-gripping the hold a limb is
     already on costs a move but shifts the torso toward it — weight-shift.
     The solver uses it to climb out of the sloper gate.
Verdict: keep. New bands in test_tuning.ml for sloper_gate.

## 2026-07-15 — Phase 4 solver: search design, bucket changes, first metrics
Hypothesis: §4.12 Dijkstra with torso/stamina bucketing (5/5) would handle
  the 20-hold ladder.
Change 1: plain Dijkstra capped out (>200k states, 44s, no solution) — the
  edge costs don't order "progress", so cheap sideways shuffles flood the
  frontier. Fix A: A* with an admissible CONSISTENT heuristic (per-limb
  vertical lower bounds + torso-rise bound of shift_factor x hand_reach per
  move; max of consistent bounds). Fix B: `solver_torso_bucket` 5 -> 10 and
  `solver_stamina_bucket` 5 -> 10 (both at the top of their documented
  ranges). Still capped: 419k states / 51s to the EXACT optimum (cost 91).
Change 2: limbs-level dominance pruning — for one (limbs, chalk) config, a
  state with more stamina and lower cost dominates regardless of torso.
  Stronger pessimism than §4.12's bucketing (documented in solver.ml); the
  ladder canary guards against over-pruning. Result: 43.7k states / 4.5s,
  cost 95 (~4% over exact). Also: Wall.find made O(1) (ids are dense array
  indices) — it's called ~10x per gate call, millions of times per solve.
Change 3: `solver_max_states` 200k -> 1M (safety cap only).
First metrics (recorded as bands in test_tuning.ml):
  ladder: cost 95, 13 moves, 0 rests(!), min_stamina 18, 0 critical, 43.7k
  states. The optimal route takes 2-rung reaches — the canary is much
  easier than its hand script (38 turns) implied. Content lesson for
  Phases 5-6: tight hold spacing lets the solver (and good players) skip
  rungs; hard walls need SPARSE holds, not just expensive ones.
  ladder @ 20 stamina: solvable, 5 rests, min_stamina 3 — the solver
  rest-manages routes. overhang: No_route in 4950 states (correct: no
  finish holds).
Verdict: keep. Bands set: ladder moves <= 20, min_stamina >= 10, chalk 0,
  critical 0, states < 200k.

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

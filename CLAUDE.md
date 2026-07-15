# CLAUDE.md — Rock-Climbing Maze Game (OCaml)

This file is the source of truth for this project. Read it fully before making changes.
It contains: (1) the game design spec, (2) the development plan, (3) instructions for how
YOU (Claude) should test, iterate, and tune the mechanics autonomously.

---

## 1. Project Overview

A turn-based rock-climbing puzzle game in OCaml. The player controls **four limbs
independently** (left hand, right hand, left foot, right foot) and must find a valid
sequence of body positions to reach the top of a climbing wall.

The wall is a maze:
- Only certain holds are useful; some routes dead-end.
- Some routes consume too much stamina or chalk.
- Some poses are reachable but poorly balanced.
- Only one or two route *families* should reach the top.

The objective is not just "move up" — it's managing limb placement, torso position,
grip difficulty, stamina, balance, and limited chalk simultaneously.

**The core interaction loop that must feel true:**

```
Poor balance  → harder grip → greater stamina drain → increased need for chalk
Chalk         → improves grip → reduces stamina drain → enables slippery routes
Good feet     → improves balance → reduces grip effort → preserves stamina and chalk
```

If a change to constants or mechanics breaks this causal chain, the change is wrong.

---

## 2. Hard Rules for Development

1. **Engine first, graphics never block progress.** The game state and all mechanics
   must be a pure, headless library (`lib/`). Rendering is a thin consumer. Early
   phases render as ASCII to stdout. Do NOT start with OCaml's `Graphics` library —
   it is a separate opam package now, is finicky on macOS, and cannot be tested
   headlessly. Add a graphical frontend only in a late phase (raylib or Bogue are
   better candidates than Graphics; decide then).

2. **One gate for all moves.** Every state transition — player input, solver edge
   expansion, generator route-building — must go through the single function
   `Movement.attempt_move`. Never duplicate validation logic. If the solver and the
   player can disagree about whether a move is legal, the project is broken.

3. **All tuning constants live in `lib/config.ml`.** No magic numbers anywhere else.
   Every constant gets a comment with its unit and its plausible range. Tuning means
   editing one file and re-running the test/tuning suite.

4. **Determinism.** All randomness goes through a seeded PRNG passed explicitly
   (`Random.State.t` or equivalent). Given a seed, generation and simulation must be
   fully reproducible. Never use global `Random.self_init`.

5. **Pick one stdlib style and keep it.** Use the OCaml **standard library** (not
   Jane Street Core) to minimize dependencies. The design doc below uses some Core
   idioms (`Int.Set.t`, `List.fold ~init`) — translate them to stdlib
   (`module IntSet = Set.Make(Int)`, `List.fold_left`) when implementing.

6. **Immutable state.** `game_state` is immutable; every turn produces a new state.
   This makes the solver, undo, replays, and testing trivial.

7. **Every phase ends with passing tests and an updated `TUNING_LOG.md`** (see §7).

---

## 3. Repository Layout

```
.
├── CLAUDE.md              # this file
├── TUNING_LOG.md          # running record of tuning experiments (see §7)
├── dune-project
├── lib/
│   ├── config.ml          # ALL tuning constants
│   ├── types.ml           # shared types (limb, point, hold, states)
│   ├── geometry.ml/.mli   # distance, averages, reach, span, horizontal range
│   ├── hold.ml/.mli       # hold kinds, compatibility, durability
│   ├── balance.ml/.mli    # support center, stability classification, fall detection
│   ├── chalk.ml/.mli      # chalk application, decay
│   ├── stamina.ml/.mli    # movement/pose/grip costs
│   ├── movement.ml/.mli   # attempt_move — THE single validation gate
│   ├── wall.ml/.mli       # wall data, hold lookup, hand-built test walls
│   ├── player.ml/.mli     # player state helpers
│   ├── game.ml/.mli       # turn loop, win/loss, status
│   ├── solver.ml/.mli     # Dijkstra over body configurations
│   ├── generator.ml/.mli  # guaranteed-route generation + decoys (late phase)
│   └── ascii.ml/.mli      # ASCII renderer (grid + HUD) — used by tests too
├── bin/
│   └── main.ml            # interactive terminal game (reads keys, prints ASCII)
└── test/
    ├── test_geometry.ml
    ├── test_movement.ml
    ├── test_balance.ml
    ├── test_scenarios.ml  # scripted climbs on hand-built walls
    ├── test_solver.ml
    └── test_tuning.ml     # invariant/property checks over constants (see §6)
```

Use `dune` for building, `alcotest` for tests. `dune runtest` must always pass on main.

---

## 4. Game Design Specification

### 4.1 Core Gameplay Loop (turn-based)

Each turn the player:
1. Selects one limb.
2. Sees which holds that limb can reach.
3. Selects a destination hold (or applies chalk, or rests).
4. The game validates the resulting pose.
5. Torso and balance are recalculated.
6. Stamina and chalk effects are applied; hold conditions update.
7. Player continues, falls, or wins.

Terminal controls (Phase 1+): `1`–`4` select limb (LH, RH, LF, RF), arrow keys or
`n`/`p` cycle reachable holds, Enter confirms, `c` chalks the selected hand, `r`
rests when allowed, `u` undo (dev convenience — trivial with immutable states).

### 4.2 Core Types (stdlib style)

```ocaml
type limb = Left_hand | Right_hand | Left_foot | Right_foot

type point = { x : float; y : float }

type hold_kind =
  | Jug | Crimp | Sloper | Foothold | Rest
  | Crumbling | Chalk_refill | Finish

type hold = {
  id : int;
  position : point;
  kind : hold_kind;
  durability : int option;      (* Some n for crumbling holds *)
}

type limb_positions = {
  left_hand : int option;       (* hold id *)
  right_hand : int option;
  left_foot : int option;
  right_foot : int option;
}

type chalk_state = {
  remaining : int;              (* uses left in bag *)
  left_hand_chalk : int;        (* turns of chalk left on hand *)
  right_hand_chalk : int;
}

type player_state = {
  limbs : limb_positions;
  torso : point;
  stamina : int;
  chalk : chalk_state;
  turn : int;
}

module IntSet = Set.Make (Int)

type game_status = Playing | Won | Fallen of string
(* Fallen carries a human-readable reason: crucial for debugging & tuning *)

type wall = {
  holds : hold array;
  width : int;
  height : int;
  finish_y : float;
}

type game_state = {
  player : player_state;
  wall : wall;
  broken_holds : IntSet.t;
  status : game_status;
}
```

**Note the change from the original doc:** `occupied_by` is NOT stored on the hold.
Occupancy is derived from `limb_positions` (single source of truth). `Fallen` carries
a reason string so tests and tuning runs can assert *why* a fall happened.

### 4.3 Hold Behavior

| Kind         | Limbs      | Base grip cost | Notes |
|--------------|------------|----------------|-------|
| Jug          | hands+feet | 1              | Big, reliable, no chalk needed |
| Crimp        | hands only | 4 (2 chalked)  | Cannot hold both hands at once |
| Sloper       | hands only | 8 (3 chalked)  | May be unusable unchalked on hard walls |
| Foothold     | feet only  | 0              | Improves balance |
| Rest         | hands+feet | 1              | Enables resting (see below) |
| Crumbling    | varies     | as base kind   | `durability` decrements each occupied turn; breaks at 0, limb detaches |
| Chalk_refill | hands      | 1              | Restores limited chalk when grabbed |
| Finish       | hands      | 3              | Win condition target |

**Resting** is allowed when: at least one hand is on a Rest hold, at least one foot is
attached, and stability is `Stable`. Resting restores `Config.rest_recovery` stamina
and consumes a turn (chalk still decays).

**Winning:** both hands on finish holds, at least one foot attached, pose valid,
stamina > 0.

### 4.4 Torso Model

The torso is its own point (not the limb average). It governs reach, balance, and cost.
After every limb move, the torso shifts automatically toward the destination:

```ocaml
let shift_toward torso target factor =
  { x = torso.x +. factor *. (target.x -. torso.x);
    y = torso.y +. factor *. (target.y -. torso.y) }
```

with `factor = Config.torso_shift_factor` (start at 0.25).

**UX requirement (Phase 2+):** when the player highlights a candidate hold, preview
the post-move torso position and resulting stability BEFORE they confirm. Without
this preview the shift-factor feels like hidden punishment and the maze degenerates
into trial-and-error. The ASCII renderer must support drawing a "ghost" torso.

### 4.5 Reach Rules

```ocaml
let max_reach = function
  | Left_hand | Right_hand -> Config.hand_reach   (* start: 110. *)
  | Left_foot | Right_foot -> Config.foot_reach   (* start: 90.  *)
```

A placement is invalid if `distance torso hold.position > max_reach limb`.
Additionally: feet may not be placed more than `Config.max_foot_above_torso`
(start: 15.) above the torso; hands may not be more than
`Config.max_hand_below_torso` (start: 40.) below the torso.

### 4.6 Body-Span Constraints

Even if each limb individually reaches, the whole pose must be plausible:
- hand-to-hand distance ≤ `max_hand_span` (start: 150.)
- foot-to-foot distance ≤ `max_foot_span` (start: 120.)
- same-side hand-to-foot distance ≤ `max_body_length` (start: 170.)
- limbs may not cross beyond `max_cross_over` (start: 25.) — i.e. left hand may not
  be more than 25px to the right of the right hand, etc.

### 4.7 Balance System (game-friendly, not physics)

1. Collect positions of all attached limbs ("supports").
2. Support center = average of support positions.
3. `balance_distance = distance torso support_center`.
4. Classify:

```
Stable    : d < Config.stable_threshold     (start: 20.)
Strained  : d < Config.strained_threshold   (start: 40.)
Critical  : d < Config.critical_threshold   (start: 60.)
Falling   : otherwise
```

5. Horizontal support check: torso.x must lie within
   `[min support x - margin, max support x + margin]`
   with `margin = Config.balance_margin` (start: 15.).

**The climber falls when any of:** fewer than 2 limbs attached; stability = Falling;
horizontal check fails; a span constraint is violated post-move (attempt_move should
reject these preemptively — falls only happen from hold breakage or exhaustion);
stamina hits 0 while Strained/Critical.

### 4.8 Grip Difficulty

Per attached hand: `base cost (from table 4.3, chalk-adjusted) × balance multiplier`.

```
Stable   × 1.0
Strained × 1.5
Critical × 2.0
Falling  → pose invalid
```

Example: sloper (8) → chalked (3) → critical (×2.0) = 6. Chalk compensates for bad
poses but never fully fixes them.

### 4.9 Stamina System

Start: `Config.starting_stamina = 100`.

Movement costs (per move):
```
hand up 3 | hand sideways 2 | foot up 2 | foot sideways 1
large stretch (span > 0.85 × its max) +3 | cross-body move +2
```

Pose upkeep (per turn, after the move):
```
Stable +1 | Strained +3 | Critical +7
one foot detached +2 | both feet detached +8
per crimp +4 (chalked +2) | per sloper +8 (chalked +3)
```

`total_turn_cost = movement + balance + grip + stretch`.

Exhaustion: if stamina reaches 0 in a Stable pose, the player gets exactly one more
turn (to reach a Rest hold); if Strained/Critical, they fall immediately.
(First implementation may simplify to: 0 stamina = always fall. Gate the grace-turn
behind `Config.exhaustion_grace_turn : bool`.)

### 4.10 Chalk System

- `c` chalks the currently selected hand: bag `remaining - 1`, hand chalk set to
  `Config.chalk_duration` (start: 3). Chalking costs a turn.
- Each turn, per-hand chalk decays by 1 (floor 0).
- Chalk never directly improves balance — only grip costs (see 4.8).
- Starting bag: `Config.starting_chalk = 5`. Chalk_refill holds restore
  `Config.refill_amount = 3`.

### 4.11 Move Validation — the single gate

```ocaml
val attempt_move :
  wall:wall -> broken:IntSet.t -> player_state -> limb -> hold ->
  (player_state, reject_reason) result
```

Return `Error` with a *typed reason* — not just `None`. Reasons:
`Hold_broken | Hold_occupied | Wrong_limb_for_hold | Out_of_reach | Span_violation
| Would_fall | Insufficient_stamina | Needs_chalk`. The UI shows these to the player;
tests assert on them; tuning runs aggregate them. This is a deliberate upgrade from
the original doc's `option` return — do not lose the reasons.

Validation order: hold exists & not broken → not illegally occupied → limb-compatible
→ within reach → simulate limb move → shift torso → span constraints → stability ≠
Falling → compute cost → cost ≤ stamina → apply cost, chalk decay, durability ticks →
return new state.

### 4.12 Dijkstra Solver

Nodes are full-body configurations:

```ocaml
type solver_state = {
  limbs : limb_positions;
  torso_xb : int;             (* bucketed: int_of_float (x /. 5.) *)
  torso_yb : int;
  stamina_b : int;            (* bucketed by 5 — see below *)
  chalk_remaining : int;
  lh_chalk : int;
  rh_chalk : int;
  broken : IntSet.t;
}
```

**Bucket stamina too** (by `Config.solver_stamina_bucket = 5`), not just torso —
otherwise stamina 73 vs 74 are distinct states and the space explodes. When checking
feasibility, use the bucket floor (pessimistic) so the solver never claims a route
the real game rejects.

Edges = one legal action: move a limb / chalk / rest. Edge cost:
```
stamina_used + 10 × chalk_used + 5 × (1 if resulting pose Critical) + 1 (per move)
```

Neighbor generation calls `attempt_move` — the solver must never invent its own
legality rules. Use a priority queue (implement a small binary heap or use `Psq`
from opam; stdlib has none).

Why Dijkstra over BFS: BFS minimizes moves; Dijkstra minimizes weighted effort
(stamina + chalk + risk), which is what "the intended route" means in this game.

### 4.13 Maze Generation (late phase)

Never place holds purely at random. Pipeline:
1. Create a valid 4-limb starting pose near the bottom.
2. Repeatedly pick a limb and place a new hold that `attempt_move` accepts, drifting
   upward. Track stamina/chalk during generation; insert Rest / Chalk_refill holds
   when the simulated climber would need them.
3. End with a valid finish pose. The wall now has ≥1 solution *by construction*.
4. Optionally generate a second independent route.
5. Add decoy holds and false branches. Good decoys fail after several moves, not
   immediately: sloper-heavy shortcuts, chalk-wasting early routes, routes that
   over-stretch near the top, foothold deserts, crumbling dead ends, routes that
   arrive at the finish with the wrong hand arrangement.
6. Run the solver. Reject if unsolvable (bug!) or too easy.

### 4.14 Route Families & Difficulty Scoring

Route signature = set of major hold ids used. Two solutions sharing most major holds
are one family. Count families: solve → block the optimal route's key holds → solve
again → repeat. Target: 1–2 families.

```ocaml
type difficulty_metrics = {
  optimal_cost : int;
  optimal_moves : int;
  chalk_required : int;
  min_stamina_remaining : int;
  states_expanded : int;
  max_queue_size : int;
  critical_balance_turns : int;
  misleading_branches : int;
  route_families : int;
}
```

A good hard wall: 1–2 families, forced chalk decisions, several near-valid branches,
low stamina margin at finish, multiple Strained/Critical poses on the optimal route,
many states expanded, no straight vertical path.

---

## 5. Development Phases (revised from the original doc)

Work strictly in order. A phase is done when its tests pass, `bin/main.ml` runs, and
`TUNING_LOG.md` is updated. **The solver moves EARLIER than the original plan** (it
was Phase 7 there) because it is the primary tuning instrument.

### Phase 0 — Pure state machine, zero graphics
- `types.ml`, `config.ml`, `geometry.ml`, `hold.ml`, minimal `movement.ml` with only:
  hold-exists, occupancy, limb-compatibility, reach checks. Fixed torso (average of
  limbs for now). No stamina/balance/chalk.
- One hand-built wall in `wall.ml` (`Wall.test_wall_ladder` — a trivially climbable
  jug ladder).
- `bin/main.ml` runs a **hardcoded move script** and prints limb coordinates per turn.
- Tests: geometry unit tests; movement accepts/rejects with correct `reject_reason`.

### Phase 1 — ASCII rendering + interactive play
- `ascii.ml`: render the wall as a character grid. Suggested glyphs:
  `J` jug, `c` crimp, `s` sloper, `.` foothold, `R` rest, `!` crumbling, `*` chalk
  refill, `F` finish, `T` torso, `h/H` left/right hand, `f/Q` left/right foot, lines
  optional. HUD line: turn, stamina, chalk, stability, last reject reason.
- Interactive loop in `bin/main.ml` (raw-ish terminal input is fine; even
  line-buffered "type `2 17` to move right hand to hold 17" is acceptable at first).
- **This is the stick-figure MVP the project owner asked for.**

### Phase 2 — Torso, spans, balance
- Real torso with `shift_toward`; reach measured from torso; span constraints;
  balance classification; fall reasons; horizontal support check.
- Ghost-torso preview in the ASCII renderer.
- Debug HUD must show: torso pos, support center pos, balance_distance, stability.
- Tests: scripted poses with known expected stability (see §6.2).

### Phase 3 — Stamina + grip
- Movement costs, pose upkeep, grip costs with balance multiplier, exhaustion falls,
  Rest holds + resting action.
- Tests: cost accounting on scripted climbs; exhaustion behavior both with and
  without the grace turn.

### Phase 4 — Solver (moved up!)
- Dijkstra as specced in 4.12, path reconstruction, and a `solve` CLI subcommand:
  `dune exec bin/main.exe -- solve --wall ladder --seed 42` prints the optimal move
  list, cost, and `difficulty_metrics`.
- Solver replay: feed the solver's move list back through the real game loop and
  assert it wins. **This test is the keystone of the whole project** — it proves the
  solver and game agree.
- From here on, the solver is your tuning instrument (§6.3).

### Phase 5 — Chalk + special holds
- Chalk bag, per-hand duration, refill holds, crumbling holds with durability,
  finish conditions. Extend solver state accordingly (it was designed for this).
- Tests: chalk math; crumbling-hold breakage mid-climb; solver finds chalk-dependent
  routes.

### Phase 6 — Generation + difficulty
- Pipeline from 4.13, route families from 4.14, difficulty scoring, accept/reject
  loop over seeds.
- Tests: 100 seeds → 100% of accepted walls are solver-solvable; family counts in
  range; difficulty metrics within target bands.

### Phase 7 (optional/stretch) — Graphical frontend
- raylib (via `raylib-ocaml`) or Bogue. Pure consumer of `lib/`. Colors: jug blue,
  crimp orange, sloper purple, foothold gray, rest green, crumbling red, refill
  white, finish gold. Mouse: click limb → highlight reachable → click hold.

**Cut list if time runs short (cut top-first):** graphical frontend, moving holds,
multiple levels, two route families, manual torso control, solver visualization,
crumbling holds. The game is complete with one hand-designed wall + solver.

---

## 6. How Claude Should Test, Iterate, and Tune (IMPORTANT)

You cannot playtest with human hands or eyes. Everything below exists so you can
evaluate feel and difficulty *from data*. Follow it.

### 6.1 Always-available instruments

- **ASCII snapshots.** `Ascii.render : game_state -> string` is pure. Print it in
  test failures and after every scripted move when debugging. You CAN read these —
  use them. When a scenario test fails, dump the board; don't guess from numbers.
- **Typed reject reasons + fall reasons.** Never debug from a bare `Error`. If you
  find yourself unsure why a move failed, the reason type is too coarse — extend it.
- **Deterministic replays.** Every interactive session and every solver run can be
  reduced to `(wall, seed, move list)`. Store failing cases as scenario tests.

### 6.2 Scenario tests (the backbone)

In `test/test_scenarios.ml`, keep a library of small hand-built walls, each encoding
one mechanic:

- `ladder` — jug ladder, trivially winnable. Sanity check; must always solve.
- `overhang_lean` — holds arranged so the natural route forces the torso far right
  of the supports. Asserts: a specific pose classifies as `Critical`; one further
  move is rejected `Would_fall`.
- `sloper_gate` — the only route up crosses two slopers. Asserts: unchalked attempt
  runs out of stamina; chalked attempt succeeds with ≥ N stamina left.
- `foot_desert` — a section with no footholds. Asserts: crossing it costs ≥ X more
  stamina than an equivalent section with feet.
- `crimp_pair` — asserts both-hands-on-one-crimp is rejected.
- `rest_recovery` — asserts resting works only under the required conditions.
- `crumble_trap` — a crumbling hold on a dead end; asserts breakage detaches the
  limb and, if the pose is invalidated, causes `Fallen` with the right reason.

Each scenario is a wall + a scripted move list + assertions on stability, stamina,
chalk, status, and reasons at specific turns. When any mechanic changes, these tell
you exactly which *experience* broke, not just which function.

### 6.3 Solver-driven tuning loop (your replacement for playtesting)

The solver is how you "feel" the game. After Phase 4, tune by running this loop:

1. Add a CLI subcommand `tune`:
   `dune exec bin/main.exe -- tune --walls all --report`
   For every scenario wall (and later, generated walls over a seed range), run the
   solver and print `difficulty_metrics` plus a per-turn trace of the optimal route:
   stability at each step, stamina curve, chalk usage.
2. Define **target bands** for each wall in the test suite (in `test_tuning.ml`),
   e.g. for `sloper_gate`: `chalk_required ≥ 2`, `min_stamina_remaining ∈ [5, 30]`,
   `critical_balance_turns ∈ [1, 4]`, `route_families = 1`.
3. When you change a constant in `config.ml`, run `dune runtest`. The tuning tests
   tell you which experiences drifted. Adjust, rerun, converge.
4. Record every tuning session in `TUNING_LOG.md` (§7).

**Interpretation guide — how to read the metrics like a playtester:**

| Symptom in metrics | Likely meaning | Knobs to try |
|---|---|---|
| `min_stamina_remaining` > 50 on hard walls | game too easy, stamina irrelevant | raise pose/grip costs, lower starting stamina |
| optimal route has 0 Strained/Critical turns | balance system not participating | tighten thresholds, raise `torso_shift_factor` |
| solver route zigzags limbs pointlessly | movement costs too flat | differentiate up vs sideways costs, add per-move base cost |
| `chalk_required = 0` on sloper walls | chalk pointless | raise unchalked sloper cost, or lower stamina budget |
| solver expands >10⁶ states on a 30-hold wall | state space leak | check stamina bucketing, torso bucket size, dedup |
| ladder wall becomes unsolvable | a constant crossed a hard floor | binary-search the last change; ladder must ALWAYS solve |
| every generated wall rejected as "too easy" | decoys too weak | strengthen decoy patterns in 4.13 step 5 |

### 6.4 Property tests (invariants that must never break)

Add lightweight property checks (hand-rolled loops over random seeds are fine;
`qcheck` if you prefer) in `test_tuning.ml`:

- **Solver/game agreement:** for any wall the solver solves, replaying its move list
  through `Game` ends in `Won`. (Keystone invariant.)
- **Monotonicity of chalk:** for a fixed wall+route, chalking a hand never increases
  the cost of the same move.
- **Monotonicity of balance:** for a fixed pose, moving the torso closer to the
  support center never worsens the stability class.
- **attempt_move purity:** calling it twice with the same inputs yields identical
  results (guards against hidden mutation).
- **No teleporting:** in any accepted state, every attached limb is within its
  max reach of the torso, and all span constraints hold. Check this as a
  post-condition invariant after EVERY accepted move in every test.
- **Reach asymmetry preserved:** `hand_reach > foot_reach` and feet-above-torso
  limit enforced (guards config edits).

### 6.5 Iteration protocol per mechanic

When implementing or changing a mechanic, follow this order — do not skip steps:

1. Write/update the scenario test that captures the *intended experience* first.
2. Implement against `config.ml` constants only.
3. Run scenarios; dump ASCII on failure and actually read the board.
4. Run the solver on affected walls; compare metrics against target bands.
5. If a constant needs to move outside its documented plausible range to make tests
   pass, the *mechanic design* is probably wrong — reconsider the formula, not just
   the number. Note this in `TUNING_LOG.md`.
6. Commit with a message naming the mechanic, the constants touched, and the
   before/after key metrics.

### 6.6 Things you will be tempted to do — don't

- Don't add a second "quick" legality check in the solver or generator "for speed".
  Optimize `attempt_move` itself if needed.
- Don't tune by making the ladder wall harder. The ladder is a canary, not content.
- Don't store occupancy or torso on holds/wall. Derive from player state.
- Don't let rendering functions take anything but `game_state` (+ selection UI
  state). If a renderer needs more, the state type is missing something.
- Don't silently widen a target band to make a test pass. Bands change only with a
  `TUNING_LOG.md` entry explaining why.

---

## 7. TUNING_LOG.md format

Append-only. One entry per tuning session or mechanic change:

```markdown
## 2026-07-15 — Sloper costs vs chalk value (Phase 5)
Hypothesis: unchalked slopers too cheap; solver ignores chalk on sloper_gate.
Change: Config.sloper_base 6 → 8; Config.sloper_chalked 3 (unchanged).
Result: sloper_gate chalk_required 0 → 2; min_stamina_remaining 41 → 18. ladder OK.
Verdict: keep. Bands updated: none.
```

---

## 8. Initial `config.ml` (starting values + plausible ranges)

```ocaml
(* All distances in world units (~pixels). All costs in stamina points. *)

(* Reach *)
let hand_reach = 110.            (* 90–130 *)
let foot_reach = 90.             (* 70–110; must stay < hand_reach *)
let max_foot_above_torso = 15.   (* 0–30 *)
let max_hand_below_torso = 40.   (* 20–60 *)

(* Body span *)
let max_hand_span = 150.         (* 120–180 *)
let max_foot_span = 120.         (* 90–150 *)
let max_body_length = 170.       (* 140–200 *)
let max_cross_over = 25.         (* 0–40 *)

(* Torso *)
let torso_shift_factor = 0.25    (* 0.15–0.35 *)

(* Balance thresholds (distance torso -> support center) *)
let stable_threshold = 20.       (* 15–30 *)
let strained_threshold = 40.     (* 30–50 *)
let critical_threshold = 60.     (* 50–75 *)
let balance_margin = 15.         (* 5–25; horizontal lean allowance *)

(* Grip base costs *)
let jug_grip = 1
let crimp_grip = 4               (* chalked: *)  let crimp_grip_chalked = 2
let sloper_grip = 8              (* chalked: *)  let sloper_grip_chalked = 3
let rest_grip = 1
let finish_grip = 3

(* Balance grip multipliers *)
let mult_stable = 1.0
let mult_strained = 1.5
let mult_critical = 2.0

(* Movement costs *)
let hand_up = 3     let hand_side = 2
let foot_up = 2     let foot_side = 1
let big_stretch_penalty = 3      (* applied when span > 0.85 × its max *)
let cross_body_penalty = 2

(* Pose upkeep *)
let upkeep_stable = 1   let upkeep_strained = 3   let upkeep_critical = 7
let one_foot_off = 2    let both_feet_off = 8

(* Stamina *)
let starting_stamina = 100       (* 80–120 *)
let rest_recovery = 15           (* 10–25 *)
let exhaustion_grace_turn = true

(* Chalk *)
let starting_chalk = 5           (* 3–8 *)
let chalk_duration = 3           (* 2–4 turns *)
let refill_amount = 3            (* 2–5 *)

(* Solver *)
let solver_torso_bucket = 5.     (* 3–10 *)
let solver_stamina_bucket = 5    (* 2–10 *)
let solver_chalk_edge_weight = 10
let solver_critical_edge_weight = 5
```

---

## 9. Quick command reference (once phases exist)

```
dune build                                  # must be warning-clean
dune runtest                                # all tests, all phases
dune exec bin/main.exe -- play --wall ladder
dune exec bin/main.exe -- script --wall overhang_lean --moves moves.txt
dune exec bin/main.exe -- solve --wall sloper_gate
dune exec bin/main.exe -- tune --walls all --report
dune exec bin/main.exe -- gen --seed 42 --difficulty hard   # Phase 6
```

---

## 10. Definition of Done (whole project / MVP)

- One hand-designed wall with jugs, crimps, slopers, footholds, a rest hold, and
  finish holds; at least one correct route and several misleading branches.
- Four independently controlled limbs, turn-based, with torso, reach, span, balance,
  stamina, and chalk all active.
- Win and fall conditions with human-readable reasons.
- Solver solves it; solver replay through the game wins; tuning metrics in band.
- (Stretch) Generator producing accepted walls across seeds.

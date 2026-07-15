open! Core

(* ALL tuning constants live here (CLAUDE.md rule 3). No magic numbers anywhere
   else. Every constant: unit + plausible range. Values are the CLAUDE.md §8
   starting values; Phase 0 only *uses* the Reach block, but the whole table is
   here so later phases edit values, never structure. *)

(* All distances in world units (~pixels). All costs in stamina points.
   y increases UPWARD from the wall base. *)

(* Reach *)
let hand_reach = 110. (* world units; 90–130 *)
let foot_reach = 90. (* world units; 70–110; must stay < hand_reach *)
let max_foot_above_torso = 15. (* world units; 0–30 *)
let max_hand_below_torso = 40. (* world units; 20–60 *)

(* Body span *)
let max_hand_span = 150. (* world units; 120–180 *)
let max_foot_span = 120. (* world units; 90–150 *)
let max_body_length = 170. (* world units; 140–200 *)
let max_cross_over = 25. (* world units; 0–40 *)

(* Torso *)
let torso_shift_factor = 0.25 (* unitless fraction; 0.15–0.35 *)

(* Balance thresholds (distance torso -> support center) *)
let stable_threshold = 20. (* world units; 15–30 *)
let strained_threshold = 40. (* world units; 30–50 *)
let critical_threshold = 60. (* world units; 50–75 *)
let balance_margin = 15. (* world units; 5–25; horizontal lean allowance *)

(* Grip base costs (stamina points per turn while attached) *)
let jug_grip = 1
let crimp_grip = 4
let crimp_grip_chalked = 2
let sloper_grip = 8
let sloper_grip_chalked = 3
let rest_grip = 1
let finish_grip = 3

(* Balance grip multipliers (unitless) *)
let mult_stable = 1.0
let mult_strained = 1.5
let mult_critical = 2.0

(* Movement costs (stamina points per move) *)
let hand_up = 3
let hand_side = 2
let foot_up = 2
let foot_side = 1
let big_stretch_penalty = 3 (* applied when span > 0.85 × its max *)
let cross_body_penalty = 2

(* Pose upkeep (stamina points per turn) *)
let upkeep_stable = 1
let upkeep_strained = 3
let upkeep_critical = 7
let one_foot_off = 2
let both_feet_off = 8

(* Stamina *)
let starting_stamina = 100 (* points; 80–120 *)
let rest_recovery = 15 (* points; 10–25 *)
let exhaustion_grace_turn = true

(* Chalk *)
let starting_chalk = 5 (* bag uses; 3–8 *)
let chalk_duration = 3 (* turns per application; 2–4 *)
let refill_amount = 3 (* bag uses restored; 2–5 *)

(* Solver *)
let solver_torso_bucket = 5. (* world units per bucket; 3–10 *)
let solver_stamina_bucket = 5 (* stamina points per bucket; 2–10 *)
let solver_chalk_edge_weight = 10
let solver_critical_edge_weight = 5

(* Rendering — display only, never gameplay *)
let ascii_cell = 10. (* world units per ASCII grid cell; 5–20 *)
let pixels_per_unit = 2. (* Graphics-window pixels per world unit; 1–4 *)
let window_margin_px = 20 (* Graphics-window border margin, pixels; 0–50 *)
let render_delay_s = 0.4 (* seconds between scripted moves on screen; 0.1–1.0 *)

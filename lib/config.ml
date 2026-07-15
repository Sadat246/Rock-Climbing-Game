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
let chalk_refill_grip = 1

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
let solver_torso_bucket = 10. (* world units per bucket; 3–10 *)
let solver_stamina_bucket = 10 (* stamina points per bucket; 2–10 *)
let solver_chalk_edge_weight = 10
let solver_critical_edge_weight = 5
let solver_move_edge_weight = 1 (* per-action base cost; 1–3 *)
let solver_max_states = 1_000_000 (* expansion safety cap; 50k–1M *)

(* Generation (Phase 6). Distances in world units. *)
let gen_wall_width = 140. (* 120–240 *)
let gen_wall_height = 340. (* 220–400 *)
let gen_min_hold_spacing = 18. (* 10–25; sparse = harder AND faster to solve (Phase 4 lesson) *)
let gen_rest_threshold = 40 (* insert a rest stop when sim stamina drops below; 25–60 *)
let gen_rest_target = 90 (* rest back up to; 70–100 *)
let gen_retry_limit = 40 (* placement samples per step before fallback; 20–80 *)
let gen_step_limit = 400 (* total construction steps before bailing a seed *)

(* Generation acceptance bands (§4.13 step 6 / §4.14) *)
let gen_accept_min_moves = 12 (* fewer = too easy *)
let gen_accept_max_moves = 60 (* more = a slog *)
let gen_accept_max_stamina_margin = 55 (* optimal route must arrive with <= this *)
let gen_accept_max_families = 2 (* 1–2 route families (§4.14) *)

(* Rendering — display only, never gameplay *)
let ascii_cell = 10. (* world units per ASCII grid cell; 5–20 *)
let pixels_per_unit = 2.5 (* Graphics-window pixels per world unit; 1–4 *)
let window_margin_px = 24 (* Graphics-window border margin, pixels; 0–50 *)
let hud_band_px = 64 (* height of the HUD strip above the wall, pixels; 48–96 *)
let min_window_width_px = 460 (* so HUD text never clips; 400–600 *)
let render_delay_s = 0.4 (* seconds between scripted moves on screen; 0.1–1.0 *)
let click_radius_px = 12 (* mouse hit radius around a hold, pixels; 5–20 *)

(* Hint toggles — owner decision 2026-07-15: figuring out where limbs can go
   IS the puzzle, so both default OFF. Flip on for debugging/tuning only. *)
let highlight_reachable = false (* rings on holds the selected limb could take *)
let show_move_preview = false (* ghost torso + post-move balance/cost readout *)

(* Fall animation (Graphics window only) *)
let fall_animation_frames = 16 (* tumble frames; 8–30 *)
let fall_frame_delay_s = 0.035 (* seconds per frame; 0.02–0.06 *)

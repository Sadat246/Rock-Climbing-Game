open! Core
open Climb
open Climb.Types

let px f = Int.of_float (f *. Config.pixels_per_unit)

(* The wall is centered horizontally (the window has a minimum width so HUD
   text fits); the HUD band sits above it. *)
let x_offset (wall : wall) = (Graphics.size_x () - px (Float.of_int wall.width)) / 2

let to_screen (wall : wall) (p : point) =
  x_offset wall + px p.x, Config.window_margin_px + px p.y
;;

let init (wall : wall) =
  let w =
    Int.max Config.min_window_width_px (px (Float.of_int wall.width) + (2 * Config.window_margin_px))
  in
  let h =
    px (Float.of_int wall.height) + (2 * Config.window_margin_px) + Config.hud_band_px
  in
  match Graphics.open_graph (sprintf " %dx%d" w h) with
  | () ->
    Graphics.set_window_title "Rock Climbing";
    (* Double-buffer: we draw to the backing store and flip when done. *)
    Graphics.auto_synchronize false;
    Ok ()
  | exception Graphics.Graphic_failure message -> Error message
;;

(* Palette. *)
let gold = Graphics.rgb 218 165 32
let orange = Graphics.rgb 230 130 0
let purple = Graphics.rgb 140 60 180
let hold_gray = Graphics.rgb 120 120 120
let green = Graphics.rgb 0 150 0
let pale_green = Graphics.rgb 120 200 120
let ghost_gray = Graphics.rgb 160 160 160
let hud_bg = Graphics.rgb 238 238 238
let rock_face = Graphics.rgb 196 178 152
let rock_streak = Graphics.rgb 178 160 134
let sky_top = 92, 158, 224
let sky_bottom = 224, 248 (* g, b; r is fixed at 236 in the gradient *)

let kind_color = function
  | Jug -> Graphics.rgb 60 110 205
  | Crimp -> orange
  | Sloper -> purple
  | Foothold -> hold_gray
  | Rest -> green
  | Crumbling -> Graphics.rgb 200 70 50
  | Chalk_refill -> Graphics.rgb 250 250 250
  | Finish -> gold
;;

(* ----- background: sky, sun, clouds, the rock face ----- *)

let draw_background (wall : wall) =
  let w = Graphics.size_x () in
  let h = Graphics.size_y () in
  (* sky gradient in horizontal bands *)
  let bands = 24 in
  let rt, gt, bt = sky_top in
  let gb, bb = sky_bottom in
  for i = 0 to bands - 1 do
    let t = Float.of_int i /. Float.of_int (bands - 1) in
    let lerp a b = Int.of_float (Float.of_int a +. (t *. Float.of_int (b - a))) in
    (* bottom band (i=0) is pale, top is deeper *)
    Graphics.set_color (Graphics.rgb (lerp 236 rt) (lerp gb gt) (lerp bb bt));
    let y0 = i * h / bands in
    Graphics.fill_rect 0 y0 w (h / bands + 1)
  done;
  (* sun, top-left *)
  Graphics.set_color (Graphics.rgb 255 236 160);
  Graphics.fill_circle 46 (h - 100) 26;
  Graphics.set_color (Graphics.rgb 255 214 90);
  Graphics.fill_circle 46 (h - 100) 19;
  (* clouds *)
  let cloud cx cy =
    Graphics.set_color Graphics.white;
    Graphics.fill_ellipse cx cy 26 10;
    Graphics.fill_ellipse (cx + 18) (cy + 5) 20 9;
    Graphics.fill_ellipse (cx - 18) (cy + 4) 18 8
  in
  cloud (w - 70) (h - 140);
  cloud (w / 2) (h - 220);
  (* the rock face: a slab spanning the climbable area *)
  let x0 = x_offset wall - 14 in
  let x1 = x_offset wall + px (Float.of_int wall.width) + 14 in
  Graphics.set_color rock_face;
  Graphics.fill_rect x0 0 (x1 - x0) (h - Config.hud_band_px - 6);
  (* diagonal strata streaks, deterministic *)
  Graphics.set_color rock_streak;
  Graphics.set_line_width 2;
  let rec streaks y =
    if y < h - Config.hud_band_px - 20
    then (
      Graphics.moveto x0 y;
      Graphics.lineto x1 (y + ((y * 7) mod 23) - 11);
      streaks (y + 34))
  in
  streaks 24;
  (* grass at the base *)
  Graphics.set_color (Graphics.rgb 96 160 84);
  Graphics.fill_rect 0 0 w 16
;;

(* ----- holds as rocks: irregular polygons shaped by kind ----- *)

(* deterministic per-hold jitter (no RNG at draw time) *)
let jit id k lo hi =
  let m = 1000 in
  let v = (id * 7919) + (k * 104729) in
  lo + ((v mod m) * (hi - lo) / m)
;;

let rock_poly x y ~rx ~ry ~id =
  let n = 7 in
  Array.init n ~f:(fun i ->
    let a = Float.of_int i *. 2. *. Float.pi /. Float.of_int n in
    let wobble k = Float.of_int (jit id (i + k) 75 118) /. 100. in
    ( x + Int.of_float (Float.of_int rx *. Float.cos a *. wobble 0)
    , y + Int.of_float (Float.of_int ry *. Float.sin a *. wobble 3) ))
;;

let outline poly =
  Graphics.set_color (Graphics.rgb 70 60 50);
  Graphics.set_line_width 1;
  Graphics.draw_poly poly
;;

let draw_hold (wall : wall) ~broken (h : hold) =
  if not (Set.mem broken h.id)
  then (
    let x, y = to_screen wall h.position in
    let id = h.id in
    Graphics.set_color (kind_color h.kind);
    match h.kind with
    | Jug | Rest ->
      (* big grippy blob *)
      let p = rock_poly x y ~rx:8 ~ry:7 ~id in
      Graphics.fill_poly p;
      outline p;
      if [%equal: hold_kind] h.kind Rest
      then (
        (* a ledge you can lean on: highlight strip *)
        Graphics.set_color (Graphics.rgb 180 240 180);
        Graphics.fill_rect (x - 5) (y + 2) 10 3)
    | Crimp ->
      (* thin edge: flat sliver *)
      let p =
        [| x - 8, y - 2; x + 8, y - 2; x + 9, y + 1; x - 9, y + 1 |]
      in
      Graphics.fill_poly p;
      outline p
    | Sloper ->
      (* rounded dome, nothing to wrap fingers over *)
      let n = 8 in
      let p =
        Array.init (n + 2) ~f:(fun i ->
          if i >= n
          then (if i = n then x + 11, y - 2 else x - 11, y - 2)
          else (
            let a = Float.pi *. Float.of_int i /. Float.of_int (n - 1) in
            x + Int.of_float (11. *. Float.cos a), y - 2 + Int.of_float (8. *. Float.sin a)))
      in
      Graphics.fill_poly p;
      outline p
    | Foothold ->
      let p = rock_poly x y ~rx:4 ~ry:3 ~id in
      Graphics.fill_poly p;
      outline p
    | Crumbling ->
      let p = rock_poly x y ~rx:8 ~ry:7 ~id in
      Graphics.fill_poly p;
      outline p;
      (* cracks *)
      Graphics.set_color (Graphics.rgb 90 30 20);
      Graphics.set_line_width 1;
      Graphics.moveto (x - 4) (y + 5);
      Graphics.lineto x (y - 1);
      Graphics.lineto (x - 2) (y - 6);
      Graphics.moveto (x + 5) (y + 3);
      Graphics.lineto (x + 1) (y - 2)
    | Chalk_refill ->
      let p = rock_poly x y ~rx:7 ~ry:6 ~id in
      Graphics.set_color hold_gray;
      Graphics.fill_poly p;
      outline p;
      (* chalk dusting *)
      Graphics.set_color Graphics.white;
      Graphics.fill_ellipse x (y + 1) 4 3
    | Finish ->
      (* the summit ledge *)
      let p =
        [| x - 11, y - 3; x + 11, y - 3; x + 13, y + 4; x - 13, y + 4 |]
      in
      Graphics.fill_poly p;
      outline p;
      Graphics.set_color (Graphics.rgb 255 230 120);
      Graphics.fill_rect (x - 9) (y + 1) 18 2)
;;

(* ----- the climber: articulated 2D figure with elbows and knees ----- *)

(* Two-segment limb: given anchor A (shoulder/hip) and end E (the hold),
   place the joint so both segments have display length [seg]; if the hold
   is farther than 2*seg the limb draws straight (never visually
   overstretched — display only, gameplay reach unchanged). [bend] picks
   which side the joint bulges toward. *)
let draw_two_segment ~(ax : int) ~(ay : int) ~(ex : int) ~(ey : int) ~seg ~bend =
  let dxf = Float.of_int (ex - ax) in
  let dyf = Float.of_int (ey - ay) in
  let d = Float.max 1. (Float.hypot dxf dyf) in
  let seg' = Float.max seg (d /. 2. +. 0.5) in
  let k = Float.sqrt (Float.max 0. ((seg' *. seg') -. (d *. d /. 4.))) in
  (* Clamp the bulge: a real climber's bent joint reads as a slight kink,
     not a fully-folded triangle. Straight when at full stretch. *)
  let k = Float.min k (10. +. (d *. 0.10)) in
  (* unit perpendicular *)
  let px_ = -.dyf /. d in
  let py_ = dxf /. d in
  let jx = Float.of_int (ax + ex) /. 2. +. (bend *. k *. px_) in
  let jy = Float.of_int (ay + ey) /. 2. +. (bend *. k *. py_) in
  Graphics.moveto ax ay;
  Graphics.lineto (Int.of_float jx) (Int.of_float jy);
  Graphics.lineto ex ey
;;

let shirt = Graphics.rgb 200 60 50
let pants = Graphics.rgb 50 70 130
let skin = Graphics.rgb 235 190 150

let draw_climber (wall : wall) (player : player_state) ~fallen ~won =
  let tx, ty = to_screen wall player.torso in
  let half = px Config.torso_half_height in
  let shoulder_y = ty + half in
  let hip_y = ty - half in
  let sw = px Config.shoulder_half_width in
  let hw = px Config.hip_half_width in
  let arm_seg = Float.of_int (px Config.arm_segment) in
  let leg_seg = Float.of_int (px Config.leg_segment) in
  let tint c = if fallen then Graphics.red else if won then gold else c in
  let limb_end limb =
    Option.map (Player.limb_hold_id player.limbs limb) ~f:(fun id ->
      to_screen wall (Wall.position_exn wall id))
  in
  (* legs first (behind torso) *)
  Graphics.set_color (tint pants);
  Graphics.set_line_width 3;
  List.iter [ Left_foot, tx - hw, -1.; Right_foot, tx + hw, 1. ] ~f:(fun (limb, ax, bend) ->
    match limb_end limb with
    | None -> ()
    | Some (ex, ey) ->
      (* knees bend outward-down *)
      draw_two_segment ~ax ~ay:hip_y ~ex ~ey ~seg:leg_seg ~bend);
  (* torso *)
  Graphics.set_color (tint shirt);
  Graphics.set_line_width 7;
  Graphics.moveto tx hip_y;
  Graphics.lineto tx shoulder_y;
  (* arms *)
  Graphics.set_line_width 3;
  List.iter [ Left_hand, tx - sw, 1.; Right_hand, tx + sw, -1. ] ~f:(fun (limb, ax, bend) ->
    match limb_end limb with
    | None -> ()
    | Some (ex, ey) ->
      (* elbows drop below the line to the hold *)
      Graphics.set_color (tint shirt);
      draw_two_segment ~ax ~ay:shoulder_y ~ex ~ey ~seg:arm_seg ~bend;
      (* hand *)
      Graphics.set_color (tint skin);
      Graphics.fill_circle ex ey 2);
  (* head *)
  Graphics.set_color (tint skin);
  Graphics.fill_circle tx (shoulder_y + 7) 5;
  Graphics.set_color (Graphics.rgb 120 70 40);
  Graphics.fill_ellipse tx (shoulder_y + 10) 5 2
;;

let draw_scene (gs : game_state) =
  Graphics.clear_graph ();
  draw_background gs.wall;
  Array.iter gs.wall.holds ~f:(draw_hold gs.wall ~broken:gs.broken_holds);
  let fallen, won =
    match gs.status with
    | Fallen _ -> true, false
    | Won -> false, true
    | Playing -> false, false
  in
  draw_climber gs.wall gs.player ~fallen ~won
;;

let draw (gs : game_state) =
  draw_scene gs;
  Graphics.synchronize ()
;;

let stability_color = function
  | Stable -> green
  | Strained -> orange
  | Critical -> Graphics.red
  | Falling -> Graphics.rgb 120 0 0
;;

(* Post-move state + cost for the highlighted target, via the single gate
   (pure). Desperate targets preview too — the player must see the fall
   coming. *)
let preview (gs : game_state) (ui : Ui.t) =
  Option.bind (Ui.target ui) ~f:(fun id ->
    Option.bind (Wall.find gs.wall id) ~f:(fun hold ->
      Movement.preview_move ~wall:gs.wall ~broken:gs.broken_holds gs.player ui.limb hold
      |> Result.ok))
;;

let balance_summary (gs : game_state) ~limbs ~(torso : point) =
  match Balance.report gs.wall limbs ~torso with
  | None -> "no supports", Falling
  | Some r ->
    ( sprintf
        !"torso (%.0f,%.0f)  support (%.0f,%.0f)  d %.1f  %{sexp:stability}"
        torso.x
        torso.y
        r.support_center.x
        r.support_center.y
        r.balance_distance
        r.stability
    , r.stability )
;;

let draw_hud (gs : game_state) (ui : Ui.t) ~after =
  let w = Graphics.size_x () in
  let h = Graphics.size_y () in
  Graphics.set_color hud_bg;
  Graphics.fill_rect 0 (h - Config.hud_band_px) w Config.hud_band_px;
  Graphics.set_color (Graphics.rgb 200 200 200);
  Graphics.set_line_width 1;
  Graphics.moveto 0 (h - Config.hud_band_px);
  Graphics.lineto w (h - Config.hud_band_px);
  let line i color text =
    Graphics.set_color color;
    Graphics.moveto 8 (h - 16 - (i * 14));
    Graphics.draw_string text
  in
  line
    0
    Graphics.black
    (sprintf
       !"turn %d   %{sexp:limb}%s   %{sexp:game_status}"
       gs.player.turn
       ui.limb
       (match Ui.target ui with
        | None -> " (no reachable hold)"
        | Some id -> sprintf " -> hold %d" id)
       gs.status);
  line 1 Graphics.black ui.message;
  let now_line, now_stab =
    balance_summary gs ~limbs:gs.player.limbs ~torso:gs.player.torso
  in
  line
    2
    (stability_color now_stab)
    (sprintf "now:    stamina %d   %s" gs.player.stamina now_line);
  match after with
  | None -> ()
  | Some ((p : player_state), (cost : Movement.cost)) ->
    let after_line, after_stab = balance_summary gs ~limbs:p.limbs ~torso:p.torso in
    if cost.total > gs.player.stamina
    then
      line
        3
        Graphics.red
        (sprintf
           "after:  cost %d > stamina %d = FALL   %s"
           cost.total
           gs.player.stamina
           after_line)
    else
      line
        3
        (stability_color after_stab)
        (sprintf "after:  cost %d -> stamina %d   %s" cost.total p.stamina after_line)
;;

let draw_with_ui (gs : game_state) (ui : Ui.t) =
  draw_scene gs;
  let wall = gs.wall in
  (* Owner-disabled hint (Config.highlight_reachable): rings on the holds the
     selected limb could actually take. Debug/tuning only. *)
  if Config.highlight_reachable
  then (
    Graphics.set_color pale_green;
    Graphics.set_line_width 1;
    Array.iter wall.holds ~f:(fun h ->
      match
        Movement.preview_move ~wall ~broken:gs.broken_holds gs.player ui.limb h
      with
      | Error _ -> ()
      | Ok _ ->
        let x, y = to_screen wall h.position in
        Graphics.draw_circle x y 8));
  (* Selected limb's line redrawn in green, thicker. *)
  Graphics.set_color green;
  Graphics.set_line_width 3;
  Option.iter (Player.limb_hold_id gs.player.limbs ui.limb) ~f:(fun id ->
    let tx, ty = to_screen wall gs.player.torso in
    let x, y = to_screen wall (Wall.position_exn wall id) in
    Graphics.moveto tx ty;
    Graphics.lineto x y);
  (* Cursor ring: just where you're pointing — says nothing about whether the
     move is possible. *)
  Graphics.set_color green;
  Graphics.set_line_width 2;
  Option.iter (Ui.target ui) ~f:(fun id ->
    let x, y = to_screen wall (Wall.position_exn wall id) in
    Graphics.draw_circle x y 9);
  (* Ghost torso preview: owner-disabled hint (Config.show_move_preview). *)
  let after = if Config.show_move_preview then preview gs ui else None in
  Option.iter after ~f:(fun ((p : player_state), (_ : Movement.cost)) ->
    let gx, gy = to_screen wall p.torso in
    Graphics.set_color ghost_gray;
    Graphics.set_line_width 1;
    Graphics.draw_circle gx gy 6;
    let tx, ty = to_screen wall gs.player.torso in
    Graphics.moveto tx ty;
    Graphics.lineto gx gy);
  draw_hud gs ui ~after;
  Graphics.synchronize ()
;;

(* The tumble: a red figure with flailing limbs accelerating from the
   pre-fall torso position down to the start pose. Pure display — the game
   state has already been reset by the time this runs. *)
let animate_fall ~(from : game_state) ~(landed : game_state) =
  let wall = landed.wall in
  let src = from.player.torso in
  let dst = landed.player.torso in
  let frames = Config.fall_animation_frames in
  for i = 1 to frames do
    let t = Float.of_int i /. Float.of_int frames in
    let pos =
      (* gravity: ease-in on the vertical, linear drift on the horizontal *)
      { x = src.x +. ((dst.x -. src.x) *. t); y = src.y +. ((dst.y -. src.y) *. t *. t) }
    in
    Graphics.clear_graph ();
    draw_background wall;
    Array.iter wall.holds ~f:(draw_hold wall ~broken:landed.broken_holds);
    let x, y = to_screen wall pos in
    Graphics.set_color Graphics.red;
    Graphics.set_line_width 2;
    let spin = Float.of_int i *. 0.8 in
    List.iter [ 0.4; 1.9; 3.5; 5.1 ] ~f:(fun limb_angle ->
      let a = spin +. limb_angle in
      Graphics.moveto x y;
      Graphics.lineto
        (x + Int.of_float (13. *. Float.cos a))
        (y + Int.of_float (13. *. Float.sin a)));
    Graphics.fill_circle x y 5;
    Graphics.synchronize ();
    ignore (Core_unix.nanosleep Config.fall_frame_delay_s : float)
  done
;;

(* Screen-position -> nearest hold within the click radius. *)
let hold_at (wall : wall) ~mouse_x ~mouse_y =
  Array.fold wall.holds ~init:None ~f:(fun best h ->
    let x, y = to_screen wall h.position in
    let d = Float.hypot (Float.of_int (x - mouse_x)) (Float.of_int (y - mouse_y)) in
    if Float.( <= ) d (Float.of_int Config.click_radius_px)
    then (
      match best with
      | Some (_, best_d) when Float.( <= ) best_d d -> best
      | Some _ | None -> Some (h.id, d))
    else best)
  |> Option.map ~f:fst
;;

type event =
  | Key of char
  | Click of int option (* hold id under the pointer, if any *)
  | Hover of int option

let next_event (wall : wall) =
  let st =
    Graphics.wait_next_event
      [ Graphics.Button_down; Graphics.Key_pressed; Graphics.Mouse_motion ]
  in
  if st.keypressed
  then Key (Graphics.read_key ())
  else if st.button
  then Click (hold_at wall ~mouse_x:st.mouse_x ~mouse_y:st.mouse_y)
  else Hover (hold_at wall ~mouse_x:st.mouse_x ~mouse_y:st.mouse_y)
;;

let read_key () = Graphics.read_key ()
let wait_for_key () = ignore (Graphics.read_key () : char)
let close () = Graphics.close_graph ()

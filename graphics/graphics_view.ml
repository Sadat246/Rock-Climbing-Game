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

(* Hold colors per CLAUDE.md §7. *)
let gold = Graphics.rgb 218 165 32
let orange = Graphics.rgb 230 130 0
let purple = Graphics.rgb 140 60 180
let hold_gray = Graphics.rgb 120 120 120
let green = Graphics.rgb 0 150 0
let pale_green = Graphics.rgb 120 200 120
let ghost_gray = Graphics.rgb 160 160 160
let hud_bg = Graphics.rgb 238 238 238

let kind_color = function
  | Jug -> Graphics.rgb 30 90 200
  | Crimp -> orange
  | Sloper -> purple
  | Foothold -> hold_gray
  | Rest -> green
  | Crumbling -> Graphics.red
  | Chalk_refill -> Graphics.rgb 250 250 250
  | Finish -> gold
;;

let hold_radius = function
  | Finish -> 6
  | Jug | Rest | Crimp | Sloper | Crumbling | Chalk_refill -> 5
  | Foothold -> 3
;;

let draw_hold (wall : wall) ~broken (h : hold) =
  if not (Set.mem broken h.id)
  then (
    let x, y = to_screen wall h.position in
    let r = hold_radius h.kind in
    Graphics.set_color (kind_color h.kind);
    Graphics.fill_circle x y r;
    (* outline so pale holds (chalk refill) stay visible *)
    Graphics.set_color Graphics.black;
    Graphics.set_line_width 1;
    Graphics.draw_circle x y r;
    (* finish holds get a celebratory double ring *)
    match h.kind with
    | Finish ->
      Graphics.set_color gold;
      Graphics.draw_circle x y (r + 3)
    | Jug | Crimp | Sloper | Foothold | Rest | Crumbling | Chalk_refill -> ())
;;

let draw_climber (wall : wall) (player : player_state) ~color =
  let tx, ty = to_screen wall player.torso in
  Graphics.set_color color;
  Graphics.set_line_width 2;
  (* Stick figure: a line from the torso to each attached limb. *)
  List.iter (Player.attached player.limbs) ~f:(fun (_, hold_id) ->
    let x, y = to_screen wall (Wall.position_exn wall hold_id) in
    Graphics.moveto tx ty;
    Graphics.lineto x y);
  Graphics.fill_circle tx ty 6;
  (* Head *)
  Graphics.draw_circle tx (ty + 11) 5
;;

let climber_color (gs : game_state) =
  match gs.status with
  | Fallen _ -> Graphics.red
  | Won -> Graphics.rgb 218 165 32
  | Playing -> Graphics.black
;;

let draw_scene (gs : game_state) =
  Graphics.clear_graph ();
  Array.iter gs.wall.holds ~f:(draw_hold gs.wall ~broken:gs.broken_holds);
  draw_climber gs.wall gs.player ~color:(climber_color gs)
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
  (* Reachable holds for the selected limb: pale green rings, or RED when
     committing would exceed remaining stamina (= a fall). *)
  Graphics.set_line_width 1;
  List.iter ui.candidates ~f:(fun id ->
    Graphics.set_color (if Ui.is_desperate ui id then Graphics.red else pale_green);
    let x, y = to_screen wall (Wall.position_exn wall id) in
    Graphics.draw_circle x y 8);
  (* Selected limb's line redrawn in green, thicker. *)
  Graphics.set_color green;
  Graphics.set_line_width 3;
  Option.iter (Player.limb_hold_id gs.player.limbs ui.limb) ~f:(fun id ->
    let tx, ty = to_screen wall gs.player.torso in
    let x, y = to_screen wall (Wall.position_exn wall id) in
    Graphics.moveto tx ty;
    Graphics.lineto x y);
  (* Highlighted target hold: bold ring (red when desperate). *)
  Graphics.set_line_width 2;
  Option.iter (Ui.target ui) ~f:(fun id ->
    Graphics.set_color (if Ui.is_desperate ui id then Graphics.red else green);
    let x, y = to_screen wall (Wall.position_exn wall id) in
    Graphics.draw_circle x y 9);
  (* Ghost torso: where the torso would shift if the move is confirmed. *)
  let after = preview gs ui in
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

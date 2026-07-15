open! Core
open Climb
open Climb.Types

let px f = Int.of_float (f *. Config.pixels_per_unit)
let to_screen (p : point) = Config.window_margin_px + px p.x, Config.window_margin_px + px p.y

let init (wall : wall) =
  let w = px (Float.of_int wall.width) + (2 * Config.window_margin_px) in
  let h = px (Float.of_int wall.height) + (2 * Config.window_margin_px) in
  match Graphics.open_graph (sprintf " %dx%d" w h) with
  | () ->
    Graphics.set_window_title "Rock Climbing — Phase 0";
    (* Double-buffer: we draw to the backing store and flip in [draw]. *)
    Graphics.auto_synchronize false;
    Ok ()
  | exception Graphics.Graphic_failure message -> Error message
;;

let draw_hold ~broken (h : hold) =
  if not (Set.mem broken h.id)
  then (
    let x, y = to_screen h.position in
    match h.kind with
    | Finish ->
      Graphics.set_color Graphics.red;
      Graphics.fill_circle x y 5
    | Jug | Crimp | Sloper | Foothold | Rest | Crumbling | Chalk_refill ->
      Graphics.set_color Graphics.black;
      Graphics.fill_circle x y 3)
;;

let draw_climber (wall : wall) (player : player_state) =
  let tx, ty = to_screen player.torso in
  Graphics.set_color Graphics.blue;
  (* Stick figure: a line from the torso to each attached limb. *)
  List.iter (Player.attached player.limbs) ~f:(fun (_, hold_id) ->
    let x, y = to_screen (Wall.position_exn wall hold_id) in
    Graphics.moveto tx ty;
    Graphics.lineto x y);
  Graphics.fill_circle tx ty 5;
  (* Head *)
  Graphics.draw_circle tx (ty + 9) 4
;;

let draw (gs : game_state) =
  Graphics.clear_graph ();
  Array.iter gs.wall.holds ~f:(draw_hold ~broken:gs.broken_holds);
  draw_climber gs.wall gs.player;
  Graphics.synchronize ()
;;

let wait_for_key () = ignore (Graphics.read_key () : char)
let close () = Graphics.close_graph ()

open! Core
open Types

let glyph_of_kind = function
  | Finish -> 'F'
  | Jug | Crimp | Sloper | Foothold | Rest | Crumbling | Chalk_refill -> '.'
;;

let limb_glyph = function
  | Left_hand -> 'h'
  | Right_hand -> 'H'
  | Left_foot -> 'f'
  | Right_foot -> 'Q'
;;

let render (gs : game_state) =
  let wall = gs.wall in
  let cell_of f = Int.of_float (Float.round_nearest (f /. Config.ascii_cell)) in
  let cols = cell_of (Float.of_int wall.width) + 1 in
  let rows = cell_of (Float.of_int wall.height) + 1 in
  let grid = Array.make_matrix ~dimx:rows ~dimy:cols ' ' in
  let plot (p : point) ch =
    let c = cell_of p.x in
    let r = rows - 1 - cell_of p.y in
    if r >= 0 && r < rows && c >= 0 && c < cols then grid.(r).(c) <- ch
  in
  Array.iter wall.holds ~f:(fun h ->
    if not (Set.mem gs.broken_holds h.id) then plot h.position (glyph_of_kind h.kind));
  plot gs.player.torso 'T';
  List.iter (Player.attached gs.player.limbs) ~f:(fun (limb, id) ->
    plot (Wall.position_exn wall id) (limb_glyph limb));
  let board =
    Array.to_list grid
    |> List.map ~f:(fun row -> String.rstrip (String.of_array row))
  in
  let status =
    sprintf
      !"turn %d  status %{sexp:game_status}"
      gs.player.turn
      gs.status
  in
  String.concat ~sep:"\n" (board @ [ status ])
;;

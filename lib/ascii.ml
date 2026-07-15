open! Core
open Types

(* Glyphs per CLAUDE.md Phase 1 table. *)
let glyph_of_kind = function
  | Jug -> 'J'
  | Crimp -> 'c'
  | Sloper -> 's'
  | Foothold -> '.'
  | Rest -> 'R'
  | Crumbling -> '!'
  | Chalk_refill -> '*'
  | Finish -> 'F'
;;

let limb_glyph = function
  | Left_hand -> 'h'
  | Right_hand -> 'H'
  | Left_foot -> 'f'
  | Right_foot -> 'Q'
;;

(* Board glyphs, drawn in this order (later wins a shared cell):
   holds, ghost torso 't', torso 'T', limbs, then the highlighted target '@'. *)
let board ?target ?ghost_torso (gs : game_state) =
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
  Option.iter ghost_torso ~f:(fun p -> plot p 't');
  plot gs.player.torso 'T';
  List.iter (Player.attached gs.player.limbs) ~f:(fun (limb, id) ->
    plot (Wall.position_exn wall id) (limb_glyph limb));
  Option.iter target ~f:(fun id -> plot (Wall.position_exn wall id) '@');
  Array.to_list grid |> List.map ~f:(fun row -> String.rstrip (String.of_array row))
;;

let status_line (gs : game_state) =
  sprintf
    !"turn %d  stamina %d  chalk %d L%d R%d  status %{sexp:game_status}"
    gs.player.turn
    gs.player.stamina
    gs.player.chalk.remaining
    gs.player.chalk.left_hand_chalk
    gs.player.chalk.right_hand_chalk
    gs.status
;;

let render gs = String.concat ~sep:"\n" (board gs @ [ status_line gs ])

(* Post-move state + cost for the highlighted target, via the single gate
   (pure). Desperate targets preview too — that's how the player sees the
   fall coming. *)
let preview (gs : game_state) (ui : Ui.t) =
  Option.bind (Ui.target ui) ~f:(fun id ->
    Option.bind (Wall.find gs.wall id) ~f:(fun hold ->
      Movement.preview_move ~wall:gs.wall ~broken:gs.broken_holds gs.player ui.limb hold
      |> Result.ok))
;;

let balance_summary (gs : game_state) ~limbs ~torso =
  match Balance.report gs.wall limbs ~torso with
  | None -> "no supports"
  | Some r ->
    sprintf
      !"torso (%.0f,%.0f)  support (%.0f,%.0f)  d %.1f  %{sexp:stability}"
      torso.x
      torso.y
      r.support_center.x
      r.support_center.y
      r.balance_distance
      r.stability
;;

let render_with_ui gs (ui : Ui.t) =
  let target = Ui.target ui in
  (* Hints are owner-disabled by default (Config): no ghost torso, no
     post-move readout — attempting a hold is how you learn. *)
  let after = if Config.show_move_preview then preview gs ui else None in
  let ghost_torso = Option.map after ~f:(fun ((p : player_state), _) -> p.torso) in
  let target_line =
    match target with
    | None -> sprintf !"limb %{sexp:limb}  target -" ui.limb
    | Some id ->
      let kind =
        match Wall.find gs.wall id with
        | Some h -> Sexp.to_string [%sexp (h.kind : hold_kind)]
        | None -> "?"
      in
      sprintf !"limb %{sexp:limb}  target @ hold %d (%s)" ui.limb id kind
  in
  let balance_line =
    sprintf "now:   %s" (balance_summary gs ~limbs:gs.player.limbs ~torso:gs.player.torso)
  in
  let preview_lines =
    match after with
    | None -> []
    | Some ((p : player_state), (cost : Movement.cost)) ->
      let affordability =
        if cost.total > gs.player.stamina
        then sprintf "cost %d > stamina %d — YOU WILL FALL" cost.total gs.player.stamina
        else sprintf "cost %d -> stamina %d" cost.total p.stamina
      in
      [ sprintf
          "after: %s  %s"
          (balance_summary gs ~limbs:p.limbs ~torso:p.torso)
          affordability
      ]
  in
  String.concat
    ~sep:"\n"
    (board ?target ?ghost_torso gs
     @ [ status_line gs; target_line; balance_line ]
     @ preview_lines
     @ [ ui.message ])
;;

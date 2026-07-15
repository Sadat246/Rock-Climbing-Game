open! Core
open Climb
open Climb.Types

(* Interactive game (Phase 1). Controls, both frontends:
     1-4  select limb (LH, RH, LF, RF)
     n/p  cycle the highlighted reachable hold
     m    confirm the move (Enter/space also work in the window)
     u    undo    q  quit
   ASCII mode additionally accepts a two-token direct move: "<limb#> <hold_id>".

   Flags:
     --demo     replay the hardcoded Phase 0 ladder script instead of playing
     --ascii    force the terminal frontend (no window)
     --no-wait  demo only: don't block on a keypress at the end
   Every move still goes through Movement.attempt_move — bin has no legality
   logic of its own. *)

let limb_of_char = function
  | '1' -> Some Left_hand
  | '2' -> Some Right_hand
  | '3' -> Some Left_foot
  | '4' -> Some Right_foot
  | _ -> None
;;

let describe_move limb hold_id = function
  | `Moved _ -> sprintf !"moved %{sexp:limb} to hold %d" limb hold_id
  | `Rejected reason -> sprintf !"REJECTED: %{sexp:reject_reason}" reason
;;

(* Shared command step: returns the new session + ui. *)
let apply_command (game : Game.t) (ui : Ui.t) command =
  match command with
  | `Select limb -> game, Ui.select_limb game.current limb
  | `Cycle step -> game, (if step >= 0 then Ui.next ui else Ui.prev ui)
  | `Undo ->
    (match Game.undo game with
     | None -> game, Ui.with_message ui "nothing to undo"
     | Some game -> game, Ui.with_message (Ui.select_limb game.current ui.limb) "undone")
  | `Move (limb, hold_id) ->
    (match game.current.status with
     | Won -> game, Ui.with_message ui "already won — u to undo, q to quit"
     | Fallen _ -> game, Ui.with_message ui "fallen — u to undo, q to quit"
     | Playing ->
       (match Game.move game limb ~hold_id with
        | `Moved game' ->
          let message =
            match game'.current.status with
            | Won -> "YOU WON! both hands on the finish — u to undo, q to quit"
            | Playing | Fallen _ -> describe_move limb hold_id (`Moved game')
          in
          game', Ui.with_message (Ui.select_limb game'.current limb) message
        | `Rejected _ as r -> game, Ui.with_message ui (describe_move limb hold_id r)))
;;

let confirm_move (ui : Ui.t) =
  match Ui.target ui with
  | None -> `None
  | Some hold_id -> `Command (`Move (ui.limb, hold_id))
;;

(* ----- Graphics frontend (keyboard + mouse) ----- *)

(* Clicking a hold: candidate -> move there; a hold one of our limbs is on ->
   select that limb; any other hold -> ask the gate why it's illegal and show
   the typed reason. Hovering a candidate points the cursor (and ghost torso
   preview) at it. *)
let click_command (gs : game_state) (ui : Ui.t) hold_id =
  if List.mem ui.candidates hold_id ~equal:Int.equal
  then `Command (`Move (ui.limb, hold_id))
  else (
    let limb_on_hold =
      List.find_map (Player.attached gs.player.limbs) ~f:(fun (limb, id) ->
        Option.some_if (id = hold_id) limb)
    in
    match limb_on_hold with
    | Some limb -> `Command (`Select limb)
    | None -> `Command (`Move (ui.limb, hold_id)))
;;

let rec graphics_loop game ui =
  let gs = Game.current_state game in
  Climb_graphics.Graphics_view.draw_with_ui gs ui;
  let command =
    match (Climb_graphics.Graphics_view.next_event gs.wall : Climb_graphics.Graphics_view.event) with
    | Key 'q' -> `Quit
    | Key 'n' -> `Command (`Cycle 1)
    | Key 'p' -> `Command (`Cycle (-1))
    | Key 'u' -> `Command `Undo
    | Key ('m' | ' ' | '\r' | '\n') -> confirm_move ui
    | Key c ->
      (match limb_of_char c with
       | Some limb -> `Command (`Select limb)
       | None -> `None)
    | Click (Some hold_id) -> click_command gs ui hold_id
    | Click None -> `None
    | Hover (Some hold_id) when List.mem ui.candidates hold_id ~equal:Int.equal ->
      `Focus hold_id
    | Hover _ -> `None
  in
  match command with
  | `Quit -> ()
  | `None -> graphics_loop game ui
  | `Focus hold_id -> graphics_loop game (Ui.focus ui hold_id)
  | `Command c ->
    let game, ui = apply_command game ui c in
    graphics_loop game ui
;;

(* ----- ASCII frontend ----- *)

let rec ascii_loop game ui =
  printf "\n%s\n> %!" (Ascii.render_with_ui (Game.current_state game) ui);
  match In_channel.input_line In_channel.stdin with
  | None -> printf "\n"
  | Some line ->
    let command =
      match String.split ~on:' ' (String.strip line) |> List.filter ~f:(Fn.non String.is_empty) with
      | [ "q" ] -> `Quit
      | [ "n" ] -> `Command (`Cycle 1)
      | [ "p" ] -> `Command (`Cycle (-1))
      | [ "u" ] -> `Command `Undo
      | [ "m" ] -> confirm_move ui
      | [ c ] when String.length c = 1 && Option.is_some (limb_of_char c.[0]) ->
        `Command (`Select (Option.value_exn (limb_of_char c.[0])))
      | [ c; hold_id ] when String.length c = 1 && Option.is_some (limb_of_char c.[0]) ->
        (match Int.of_string_opt hold_id with
         | Some hold_id ->
           `Command (`Move (Option.value_exn (limb_of_char c.[0]), hold_id))
         | None -> `None)
      | _ -> `None
    in
    (match command with
     | `Quit -> ()
     | `None -> ascii_loop game (Ui.with_message ui "commands: 1-4 n p m u q, or '<limb#> <hold_id>'")
     | `Command c ->
       let game, ui = apply_command game ui c in
       ascii_loop game ui)
;;

(* ----- Phase 0 scripted demo (kept: it exercises the reject paths) ----- *)

(* Hands climb the jug rungs two rows ahead; feet follow onto the jugs the
   hands vacate. 30-unit steps keep every move inside the torso-shift model. *)
let demo_script =
  let cycle c =
    [ Left_hand, (2 * c) + 4
    ; Right_hand, (2 * c) + 5
    ; Left_foot, (2 * c) + 2
    ; Right_foot, (2 * c) + 3
    ]
  in
  [ Left_hand, 14 (* jug (40, 240): Out_of_reach demo *)
  ; Right_foot, 18 (* finish (40, 300): Wrong_limb_for_hold demo *)
  ]
  @ List.concat_map (List.range 0 7) ~f:cycle
  @ [ Left_hand, 18; Right_hand, 19 ]
;;

let show_demo ~ascii gs =
  if ascii
  then printf "%s\n\n" (Ascii.render gs)
  else (
    Climb_graphics.Graphics_view.draw gs;
    ignore (Core_unix.nanosleep Config.render_delay_s : float))
;;

let run_demo ~ascii ~no_wait game =
  show_demo ~ascii (Game.current_state game);
  let final =
    List.fold demo_script ~init:game ~f:(fun game (limb, hold_id) ->
      match Game.move game limb ~hold_id with
      | `Rejected reason ->
        printf
          !"turn %d: %{sexp:limb} -> hold %d REJECTED (%{sexp:reject_reason})\n"
          (Game.current_state game).player.turn
          limb
          hold_id
          reason;
        game
      | `Moved game ->
        let gs = Game.current_state game in
        printf
          !"turn %d: %{sexp:limb} -> hold %d ok, torso (%.0f, %.0f)\n"
          gs.player.turn
          limb
          hold_id
          gs.player.torso.x
          gs.player.torso.y;
        show_demo ~ascii gs;
        game)
  in
  (match (Game.current_state final).status with
   | Won ->
     printf "Won: both hands on the finish after %d turns.\n" (Game.current_state final).player.turn
   | Playing | Fallen _ -> printf "Script ended without reaching the finish.\n");
  if not ascii
  then (
    if not no_wait then Climb_graphics.Graphics_view.wait_for_key ();
    Climb_graphics.Graphics_view.close ())
;;

let () =
  let args = Sys.get_argv () |> Array.to_list |> List.tl_exn in
  let flag f = List.mem args f ~equal:String.equal in
  let demo = flag "--demo" in
  let no_wait = flag "--no-wait" in
  let wall = Wall.test_wall_ladder in
  let game = Game.create ~wall ~start:Wall.ladder_start in
  let ascii =
    flag "--ascii"
    ||
    match Climb_graphics.Graphics_view.init wall with
    | Ok () -> false
    | Error message ->
      printf
        "No graphics window available (%s).\n\
         Falling back to ASCII mode. To get the window over SSH you need X\n\
         forwarding: an X server running on YOUR machine (macOS: XQuartz,\n\
         Windows: VcXsrv) and a connection made with `ssh -X`.\n\n"
        message;
      true
  in
  if demo
  then run_demo ~ascii ~no_wait game
  else (
    let ui = Ui.init (Game.current_state game) in
    if ascii
    then ascii_loop game ui
    else (
      graphics_loop game ui;
      Climb_graphics.Graphics_view.close ()))
;;

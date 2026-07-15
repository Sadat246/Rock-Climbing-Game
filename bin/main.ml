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
  | `Rejected reason ->
    sprintf !"can't move %{sexp:limb} to hold %d: %{sexp:reject_reason}" limb hold_id reason
;;

(* Shared command step: returns the new session + ui, plus the pre-fall
   state when the command ended in a fall (for the tumble animation). *)
let apply_command (game : Game.t) (ui : Ui.t) command =
  let no_fall (game, ui) = game, ui, None in
  match command with
  | `Select limb -> no_fall (game, Ui.select_limb game.current limb)
  | `Cycle step -> no_fall (game, (if step >= 0 then Ui.next ui else Ui.prev ui))
  | `Undo ->
    no_fall
      (match Game.undo game with
       | None -> game, Ui.with_message ui "nothing to undo"
       | Some game -> game, Ui.with_message (Ui.select_limb game.current ui.limb) "undone")
  | `Rest ->
    no_fall
      (match game.current.status with
       | Won | Fallen _ -> game, Ui.with_message ui "the climb is over - u undoes"
       | Playing ->
         (match Game.rest game with
          | `Rested game' ->
            let message =
              sprintf "rested: stamina %d" (Game.current_state game').player.stamina
            in
            game', Ui.with_message (Ui.select_limb game'.current ui.limb) message
          | `Rejected _ ->
            ( game
            , Ui.with_message
                ui
                "can't rest: need a hand on a Rest hold, a foot on, and Stable" )))
  | `Move (limb, hold_id) ->
    (match game.current.status with
     | Won ->
       no_fall (game, Ui.with_message ui "already won - m to restart, u to undo, q to quit")
     | Fallen _ ->
       (* unreachable: falls auto-reset to the start *)
       let game = Game.reset game in
       no_fall (game, Ui.with_message (Ui.init game.current) "back to the start - climb!")
     | Playing ->
       (match Game.move game limb ~hold_id with
        | `Moved (game', cost) ->
          let message =
            match game'.current.status with
            | Won -> "YOU WON! both hands on the finish - m to restart, q to quit"
            | Fallen reason -> sprintf "FELL: %s" reason
            | Playing ->
              sprintf
                !"moved %{sexp:limb} to hold %d (cost %d, stamina %d)"
                limb
                hold_id
                cost.total
                (Game.current_state game').player.stamina
          in
          no_fall (game', Ui.with_message (Ui.select_limb game'.current limb) message)
        | `Fell (game', reason) ->
          (* session already reset to the start; head of history = pre-fall *)
          ( game'
          , Ui.with_message
              (Ui.select_limb game'.current limb)
              (sprintf "YOU FELL: %s. Back to the start." reason)
          , List.hd game'.history )
        | `Rejected _ as r -> no_fall (game, Ui.with_message ui (describe_move limb hold_id r))))
;;

let confirm_move (ui : Ui.t) =
  match Ui.target ui with
  | None -> `None
  | Some hold_id -> `Command (`Move (ui.limb, hold_id))
;;

(* ----- Graphics frontend (keyboard + mouse) ----- *)

(* Clicking a hold one of our limbs is on selects that limb; clicking any
   other hold ATTEMPTS the move with the selected limb — no pre-screening,
   the wall itself is the teacher. *)
let click_command (gs : game_state) (ui : Ui.t) hold_id =
  let limb_on_hold =
    List.find_map (Player.attached gs.player.limbs) ~f:(fun (limb, id) ->
      Option.some_if (id = hold_id) limb)
  in
  match limb_on_hold with
  | Some limb -> `Command (`Select limb)
  | None -> `Command (`Move (ui.limb, hold_id))
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
    | Key 'r' -> `Command `Rest
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
    let game', ui, fell_from = apply_command game ui c in
    Option.iter fell_from ~f:(fun pre ->
      Climb_graphics.Graphics_view.animate_fall
        ~from:{ pre with status = Playing }
        ~landed:(Game.current_state game'));
    graphics_loop game' ui
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
      | [ "r" ] -> `Command `Rest
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
     | `None ->
       ascii_loop
         game
         (Ui.with_message ui "commands: 1-4 n p m r u q, or '<limb#> <hold_id>'")
     | `Command c ->
       (* no animation in the terminal: the message carries the fall *)
       let game, ui, (_ : game_state option) = apply_command game ui c in
       ascii_loop game ui)
;;

(* ----- Phase 0 scripted demo (kept: it exercises the reject paths) ----- *)

(* Hands climb the rungs two rows ahead; feet follow onto the rungs the
   hands vacate. 30-unit steps keep every move inside the torso-shift model.
   Rest stops at the y=120 and y=210 Rest rows: the full climb costs ~170
   stamina against a 100 budget. *)
let demo_script =
  let cycle c =
    [ `M (Left_hand, (2 * c) + 4)
    ; `M (Right_hand, (2 * c) + 5)
    ; `M (Left_foot, (2 * c) + 2)
    ; `M (Right_foot, (2 * c) + 3)
    ]
  in
  let cycles a b = List.concat_map (List.range a b) ~f:cycle in
  [ `M (Left_hand, 14) (* jug (40, 240): Out_of_reach demo *)
  ; `M (Right_foot, 18) (* finish (40, 300): Wrong_limb_for_hold demo *)
  ]
  @ cycles 0 2
  @ [ `R; `R; `R ] (* hands on the y=120 Rest row *)
  @ cycles 2 5
  @ [ `R; `R; `R; `R; `R ] (* hands on the y=210 Rest row *)
  @ cycles 5 7
  @ [ `M (Left_hand, 18); `M (Right_hand, 19) ]
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
    List.fold demo_script ~init:game ~f:(fun game action ->
      match action with
      | `R ->
        (match Game.rest game with
         | `Rejected reason ->
           printf
             !"turn %d: rest REJECTED (%{sexp:reject_reason})\n"
             (Game.current_state game).player.turn
             reason;
           game
         | `Rested game ->
           let gs = Game.current_state game in
           printf "turn %d: rested, stamina %d\n" gs.player.turn gs.player.stamina;
           show_demo ~ascii gs;
           game)
      | `M (limb, hold_id) ->
        (match Game.move game limb ~hold_id with
         | `Rejected reason ->
           printf
             !"turn %d: %{sexp:limb} -> hold %d REJECTED (%{sexp:reject_reason})\n"
             (Game.current_state game).player.turn
             limb
             hold_id
             reason;
           game
         | `Fell (game, reason) ->
           printf "FELL: %s (back to the start)\n" reason;
           game
         | `Moved (game, cost) ->
           let gs = Game.current_state game in
           printf
             !"turn %2d: %{sexp:limb} -> hold %d ok (cost %d, stamina %d), torso (%.0f, %.0f)\n"
             gs.player.turn
             limb
             hold_id
             cost.total
             gs.player.stamina
             gs.player.torso.x
             gs.player.torso.y;
           show_demo ~ascii gs;
           game))
  in
  (match (Game.current_state final).status with
   | Won ->
     printf
       "Won: both hands on the finish after %d turns, stamina %d left.\n"
       (Game.current_state final).player.turn
       (Game.current_state final).player.stamina
   | Fallen reason -> printf "Script ended FALLEN: %s\n" reason
   | Playing -> printf "Script ended without reaching the finish.\n");
  if not ascii
  then (
    if not no_wait then Climb_graphics.Graphics_view.wait_for_key ();
    Climb_graphics.Graphics_view.close ())
;;

(* ----- Solver subcommands (Phase 4) ----- *)

(* Per-turn trace of a solver route, replayed through the real Game — the
   §6.3 tuning instrument's view of "how the optimal climb feels". *)
let print_route ~wall ~start actions =
  let game = Game.create ~wall ~start in
  let final =
    List.fold actions ~init:game ~f:(fun game action ->
      let describe, game' =
        match (action : Solver.action) with
        | Rest ->
          ( "rest"
          , (match Game.rest game with
             | `Rested g -> g
             | `Rejected r ->
               raise_s [%message "solver route rest rejected" (r : reject_reason)]) )
        | Move (limb, hold_id) ->
          ( sprintf !"%-10s -> %2d" (Sexp.to_string [%sexp (limb : limb)]) hold_id
          , (match Game.move game limb ~hold_id with
             | `Moved (g, _) -> g
             | `Fell (_, reason) ->
               raise_s [%message "solver route fell" (reason : string)]
             | `Rejected r ->
               raise_s [%message "solver route rejected" (r : reject_reason)]) )
      in
      let gs = Game.current_state game' in
      printf
        !"turn %2d  %-17s stamina %3d  %{sexp:stability}\n"
        gs.player.turn
        describe
        gs.player.stamina
        (Balance.stability gs.wall gs.player.limbs ~torso:gs.player.torso);
      game')
  in
  printf !"final status: %{sexp:game_status}\n" (Game.current_state final).status
;;

(* Replay a solver route visually: the climber walks the optimal line in the
   Graphics window, one action per render_delay. Falls back to the text
   trace when no display is reachable. *)
let watch_route ~wall ~start actions =
  match Climb_graphics.Graphics_view.init wall with
  | Error message ->
    printf "no graphics window (%s) - text trace instead:\n" message;
    print_route ~wall ~start actions
  | Ok () ->
    let step game action =
      match (action : Solver.action) with
      | Rest ->
        (match Game.rest game with
         | `Rested g -> g
         | `Rejected r -> raise_s [%message "solver route rest rejected" (r : reject_reason)])
      | Move (limb, hold_id) ->
        (match Game.move game limb ~hold_id with
         | `Moved (g, _) -> g
         | `Fell (_, reason) -> raise_s [%message "solver route fell" (reason : string)]
         | `Rejected r -> raise_s [%message "solver route rejected" (r : reject_reason)])
    in
    let game = Game.create ~wall ~start in
    Climb_graphics.Graphics_view.draw (Game.current_state game);
    ignore (Core_unix.nanosleep Config.render_delay_s : float);
    let final =
      List.fold actions ~init:game ~f:(fun game action ->
        let game = step game action in
        Climb_graphics.Graphics_view.draw (Game.current_state game);
        ignore (Core_unix.nanosleep Config.render_delay_s : float);
        game)
    in
    printf
      !"route finished: %{sexp:game_status} - press any key in the window to close\n%!"
      (Game.current_state final).status;
    Climb_graphics.Graphics_view.wait_for_key ();
    Climb_graphics.Graphics_view.close ()
;;

let solve_wall ?(watch = false) name =
  match Wall.find_by_name name with
  | None ->
    printf
      "unknown wall %s (available: %s)\n"
      name
      (String.concat ~sep:", " (List.map Wall.all ~f:fst))
  | Some (wall, start) ->
    printf "=== %s ===\n" name;
    (match Solver.solve ~wall ~start with
     | No_route { states_expanded } ->
       printf "no route (search space exhausted after %d states)\n" states_expanded
     | Search_limit { states_expanded } ->
       printf "gave up at the state cap (%d states)\n" states_expanded
     | Solution { actions; metrics } ->
       print_route ~wall ~start actions;
       printf !"%{sexp:Solver.metrics}\n%!" metrics;
       if watch then watch_route ~wall ~start actions)
;;

let wall_arg args ~default =
  match List.drop_while args ~f:(fun a -> not (String.equal a "--wall")) with
  | _ :: name :: _ -> name
  | _ -> default
;;

let run_play args =
  let flag f = List.mem args f ~equal:String.equal in
  let demo = flag "--demo" in
  let no_wait = flag "--no-wait" in
  let wall, start =
    Option.value (Wall.find_by_name (wall_arg args ~default:"ladder"))
      ~default:(Wall.test_wall_ladder, Wall.ladder_start)
  in
  let game = Game.create ~wall ~start in
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

(* ----- Entry point ----- *)

let () =
  let args = Sys.get_argv () |> Array.to_list |> List.tl_exn in
  match args with
  | "solve" :: rest ->
    let watch = List.mem rest "--watch" ~equal:String.equal in
    solve_wall ~watch (wall_arg rest ~default:"ladder")
  | "tune" :: _ -> List.iter Wall.all ~f:(fun (name, _) -> solve_wall name)
  | args -> run_play args
;;

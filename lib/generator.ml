open! Core
open Types

type difficulty =
  | Easy
  | Medium
  | Hard
[@@deriving sexp_of, compare, equal]

let difficulty_of_string = function
  | "easy" -> Some Easy
  | "medium" -> Some Medium
  | "hard" -> Some Hard
  | _ -> None
;;

type generated =
  { wall : Types.wall
  ; start : Types.player_state
  ; route_holds : int list
  ; decoy_holds : int list
  ; difficulty : difficulty
  ; seed : int
  }
[@@deriving sexp_of]

(* Per-difficulty generation shape: how far apart route holds land (sparse =
   hard, per the Phase 4 lesson: tight spacing lets rungs be skipped), how
   often hand holds are crimps/slopers, and how many decoys get sprinkled. *)
let hand_dy ~difficulty =
  match difficulty with
  | Easy -> 30., 55.
  | Medium -> 40., 70.
  | Hard -> 48., 80.
;;

let special_rate = function
  | Easy -> 0.05
  | Medium -> 0.18
  | Hard -> 0.32
;;

let decoy_count = function
  | Easy -> 6
  | Medium -> 8
  | Hard -> 10
;;

(* ----- construction state ----- *)

type builder =
  { mutable holds_rev : hold list (* newest first *)
  ; mutable next_id : int
  ; width : float
  ; height : float
  }

let wall_of (b : builder) ~finish_y =
  { holds = Array.of_list (List.rev b.holds_rev)
  ; width = Int.of_float b.width
  ; height = Int.of_float b.height
  ; finish_y
  }
;;

let far_enough (b : builder) (p : point) =
  List.for_all b.holds_rev ~f:(fun h ->
    Float.( >= ) (Geometry.distance h.position p) Config.gen_min_hold_spacing)
;;

let add_hold (b : builder) ~x ~y ~kind ~durability =
  let id = b.next_id in
  b.next_id <- id + 1;
  b.holds_rev <- { id; position = { x; y }; kind; durability } :: b.holds_rev;
  id
;;

let remove_hold (b : builder) id =
  b.holds_rev <- List.filter b.holds_rev ~f:(fun h -> h.id <> id);
  if b.next_id = id + 1 then b.next_id <- id
;;

let no_broken = Set.empty (module Int)

(* Try to place a hold of [kind] at (x, y) and move [limb] onto it through
   the gate. On rejection the hold is removed again. *)
let try_placement b (player : player_state) limb ~x ~y ~kind =
  if Float.( < ) x 20.
     || Float.( > ) x (b.width -. 20.)
     || Float.( < ) y 20.
     || Float.( > ) y (b.height -. 20.)
     || not (far_enough b { x; y })
  then None
  else (
    let id = add_hold b ~x ~y ~kind ~durability:None in
    let wall = wall_of b ~finish_y:b.height in
    match Movement.attempt_move ~wall ~broken:no_broken player limb (Option.value_exn (Wall.find wall id)) with
    | Ok next -> Some (id, next)
    | Error _ ->
      remove_hold b id;
      None)
;;

(* ----- the construction climb ----- *)

let generate ?height ~seed ~difficulty () =
  let rng = Random.State.make [| seed |] in
  let width = Config.gen_wall_width in
  let height = Option.value height ~default:Config.gen_wall_height in
  let b = { holds_rev = []; next_id = 0; width; height } in
  let cx = width /. 2. in
  (* starting pose: two footholds, two jugs *)
  let lf = add_hold b ~x:(cx -. 20.) ~y:30. ~kind:Foothold ~durability:None in
  let rf = add_hold b ~x:(cx +. 20.) ~y:30. ~kind:Foothold ~durability:None in
  let lh = add_hold b ~x:(cx -. 20.) ~y:60. ~kind:Jug ~durability:None in
  let rh = add_hold b ~x:(cx +. 20.) ~y:60. ~kind:Jug ~durability:None in
  let start_limbs =
    { left_hand = Some lh; right_hand = Some rh; left_foot = Some lf; right_foot = Some rf }
  in
  let start =
    { limbs = start_limbs
    ; torso =
        Geometry.average
          [ { x = cx -. 20.; y = 60. }
          ; { x = cx +. 20.; y = 60. }
          ; { x = cx -. 20.; y = 30. }
          ; { x = cx +. 20.; y = 30. }
          ]
    ; stamina = Config.starting_stamina
    ; chalk =
        { remaining = Config.starting_chalk; left_hand_chalk = 0; right_hand_chalk = 0 }
    ; turn = 0
    }
  in
  let route = ref [ lf; rf; lh; rh ] in
  let record id = route := id :: !route in
  let player = ref start in
  let steps = ref 0 in
  let float_range lo hi = lo +. Random.State.float rng (hi -. lo) in
  let limb_pos limb =
    let wall = wall_of b ~finish_y:b.height in
    Option.map (Player.limb_hold_id !player.limbs limb) ~f:(Wall.position_exn wall)
  in
  let lowest_limb () =
    List.filter_map all_of_limb ~f:(fun l -> Option.map (limb_pos l) ~f:(fun p -> l, p.y))
    |> List.min_elt ~compare:(fun (_, a) (_, b) -> Float.compare a b)
    |> Option.value_map ~default:Left_hand ~f:fst
  in
  let pick_limb () =
    (* hands lead the route; feet follow in fewer, larger steps (every
       foothold multiplies both the player's options and the solver's
       branching, so fewer + purposeful beats many + trivial) *)
    let r = Random.State.float rng 1. in
    if Float.( < ) r 0.3
    then lowest_limb ()
    else if Float.( < ) r 0.75
    then (if Random.State.bool rng then Left_hand else Right_hand)
    else if Random.State.bool rng
    then Left_foot
    else Right_foot
  in
  (* chalk the hand if needed before a crimp/sloper grab; false if we can't *)
  let ensure_chalked limb =
    let chalk_left =
      match limb with
      | Left_hand -> !player.chalk.left_hand_chalk
      | Right_hand -> !player.chalk.right_hand_chalk
      | Left_foot | Right_foot -> 99
    in
    if chalk_left > 0
    then true
    else (
      let wall = wall_of b ~finish_y:b.height in
      match Movement.attempt_chalk ~wall ~broken:no_broken !player limb with
      | Ok next ->
        player := next;
        true
      | Error _ -> false)
  in
  (* one construction step: sample placements until one passes the gate *)
  let step_limb limb =
    let dy_lo, dy_hi = hand_dy ~difficulty in
    let from = Option.value (limb_pos limb) ~default:!player.torso in
    let rec attempt tries =
      if tries <= 0
      then false
      else (
        let dx =
          if Hold.is_hand limb then float_range (-45.) 45. else float_range (-30.) 30.
        in
        let dy =
          if Hold.is_hand limb then float_range dy_lo dy_hi else float_range 20. 45.
        in
        let x = Float.clamp_exn (from.x +. dx) ~min:20. ~max:(width -. 20.) in
        let y =
          if Hold.is_foot limb
          then
            (* feet may not land above torso + max_foot_above_torso: clamp
               below the limit so samples don't burn retries on the gate *)
            Float.min (from.y +. dy) (!player.torso.y +. Config.max_foot_above_torso -. 3.)
          else from.y +. dy
        in
        let kind =
          if Hold.is_foot limb
          then Foothold
          else if Float.( < ) (Random.State.float rng 1.) (special_rate difficulty)
          then (if Random.State.bool rng then Crimp else Sloper)
          else Jug
        in
        let kind =
          match kind with
          | (Crimp | Sloper) when not (ensure_chalked limb) -> Jug
          | k -> k
        in
        match try_placement b !player limb ~x ~y ~kind with
        | Some (id, next) ->
          record id;
          player := next;
          true
        | None -> attempt (tries - 1))
    in
    attempt Config.gen_retry_limit
  in
  (* rest stop: place a Rest hold for a hand, grab it, shake out *)
  let rest_stop () =
    let limb = if Random.State.bool rng then Left_hand else Right_hand in
    let from = Option.value (limb_pos limb) ~default:!player.torso in
    let rec place tries =
      if tries <= 0
      then false
      else (
        let x = Float.clamp_exn (from.x +. float_range (-30.) 30.) ~min:20. ~max:(width -. 20.) in
        let y = from.y +. float_range 5. 30. in
        match try_placement b !player limb ~x ~y ~kind:Rest with
        | Some (id, next) ->
          record id;
          player := next;
          true
        | None -> place (tries - 1))
    in
    if place Config.gen_retry_limit
    then (
      let wall = wall_of b ~finish_y:b.height in
      let rec shake () =
        if !player.stamina < Config.gen_rest_target
        then (
          match Movement.attempt_rest ~wall ~broken:no_broken !player with
          | Ok next ->
            player := next;
            shake ()
          | Error _ -> ())
      in
      shake ())
  in
  (* main loop *)
  let bail = ref None in
  while
    Option.is_none !bail
    && Float.( < ) !player.torso.y (height -. 90.)
    && !steps < Config.gen_step_limit
  do
    incr steps;
    if !player.stamina < Config.gen_rest_threshold then rest_stop ();
    if !player.chalk.remaining <= 1 && Float.( < ) (Random.State.float rng 1.) 0.5
    then (
      (* drop in a chalk pocket and grab it *)
      let limb = if Random.State.bool rng then Left_hand else Right_hand in
      let from = Option.value (limb_pos limb) ~default:!player.torso in
      let x = Float.clamp_exn (from.x +. float_range (-30.) 30.) ~min:20. ~max:(width -. 20.) in
      let y = from.y +. float_range 10. 35. in
      match try_placement b !player limb ~x ~y ~kind:Chalk_refill with
      | Some (id, next) ->
        record id;
        player := next
      | None -> ());
    if not (step_limb (pick_limb ()))
    then (
      (* couldn't move the chosen limb anywhere; try every limb (lowest
         first) before declaring the seed stuck *)
      let rescued =
        List.exists
          (List.sort all_of_limb ~compare:(fun a b ->
             let y l =
               Option.value_map (limb_pos l) ~default:0. ~f:(fun p -> p.y)
             in
             Float.compare (y a) (y b)))
          ~f:step_limb
      in
      if not rescued
      then bail := Some "construction got stuck: no legal placement found")
  done;
  if !steps >= Config.gen_step_limit && Float.( < ) !player.torso.y (height -. 90.)
  then bail := Some "construction ran over the step limit";
  match !bail with
  | Some why -> Or_error.error_string (sprintf "seed %d: %s" seed why)
  | None ->
    (* finish: two Finish holds above the hands, grabbed one after the other *)
    let finish_top = ref 0. in
    let place_finish limb dx =
      let rec attempt tries =
        if tries <= 0
        then false
        else (
          let x =
            Float.clamp_exn
              (!player.torso.x +. dx +. float_range (-25.) 25.)
              ~min:20.
              ~max:(width -. 20.)
          in
          let y = Float.min (height -. 22.) (!player.torso.y +. float_range 60. 100.) in
          match try_placement b !player limb ~x ~y ~kind:Finish with
          | Some (id, next) ->
            record id;
            player := next;
            finish_top := Float.max !finish_top y;
            true
          | None -> attempt (tries - 1))
      in
      attempt (Config.gen_retry_limit * 2)
    in
    if not (place_finish Left_hand (-20.) && place_finish Right_hand 20.)
    then Or_error.error_string (sprintf "seed %d: could not place the finish pose" seed)
    else (
      let finish_y = !finish_top in
      let wall_now = wall_of b ~finish_y in
      if not (Game.won ~wall:wall_now !player)
      then Or_error.error_string (sprintf "seed %d: finish pose does not win" seed)
      else (
        (* decoys: near-route sprinkles — slopers, crimps, crumbling bait *)
        let route_ids = List.rev !route in
        let decoys = ref [] in
        let wall_positions =
          List.filter_map route_ids ~f:(fun id -> Option.map (Wall.find wall_now id) ~f:(fun h -> h.position))
        in
        let anchors = Array.of_list wall_positions in
        let tries = decoy_count difficulty * 6 in
        let placed = ref 0 in
        let attempt = ref 0 in
        while !placed < decoy_count difficulty && !attempt < tries do
          incr attempt;
          let anchor = anchors.(Random.State.int rng (Array.length anchors)) in
          let x = Float.clamp_exn (anchor.x +. float_range (-55.) 55.) ~min:20. ~max:(width -. 20.) in
          let y = Float.clamp_exn (anchor.y +. float_range (-25.) 60.) ~min:25. ~max:(height -. 25.) in
          if far_enough b { x; y }
          then (
            let kind, durability =
              match Random.State.int rng 10 with
              | 0 | 1 -> Sloper, None
              | 2 | 3 -> Crimp, None
              | 4 -> Crumbling, Some (1 + Random.State.int rng 2)
              | 5 | 6 -> Foothold, None
              | _ -> Jug, None
            in
            let id = add_hold b ~x ~y ~kind ~durability in
            decoys := id :: !decoys;
            incr placed)
        done;
        Ok
          { wall = wall_of b ~finish_y
          ; start
          ; route_holds = route_ids
          ; decoy_holds = List.rev !decoys
          ; difficulty
          ; seed
          }))
;;

(* ----- §4.13 step 6: solver-based accept/reject ----- *)

type verdict =
  | Accepted of
      { metrics : Solver.metrics
      ; families : int
      ; actions : Solver.action list
      }
  | Rejected of string
[@@deriving sexp_of]

(* §4.14 route families: block the hand holds of the found route (minus the
   start pose and finish holds) and re-solve; repeat until unsolvable. *)
let count_families ~wall ~start ~first_actions =
  let hand_holds actions =
    List.filter_map actions ~f:(function
      | Solver.Move ((Left_hand | Right_hand), id) -> Some id
      | Move ((Left_foot | Right_foot), _) | Rest | Chalk _ -> None)
    |> List.filter ~f:(fun id ->
      match Wall.find wall id with
      | Some { kind = Finish; _ } -> false
      | Some _ | None -> true)
  in
  let rec go blocked actions families =
    if families >= Config.gen_accept_max_families + 1
    then families
    else (
      let blocked = Set.union blocked (Set.of_list (module Int) (hand_holds actions)) in
      match Solver.solve ~blocked ~wall ~start with
      | No_route _ | Search_limit _ -> families
      | Solution { actions = next_actions; _ } -> go blocked next_actions (families + 1))
  in
  go (Set.empty (module Int)) first_actions 1
;;

let evaluate (g : generated) =
  match Solver.solve ~blocked:(Set.empty (module Int)) ~wall:g.wall ~start:g.start with
  | No_route { states_expanded } ->
    Rejected
      (sprintf
         "UNSOLVABLE after %d states — generator bug, the route was built through the gate"
         states_expanded)
  | Search_limit { states_expanded } ->
    Rejected (sprintf "solver hit the state cap (%d)" states_expanded)
  | Solution { actions; metrics } ->
    if metrics.optimal_moves < Config.gen_accept_min_moves
    then Rejected (sprintf "too easy: %d moves" metrics.optimal_moves)
    else if metrics.optimal_moves > Config.gen_accept_max_moves
    then Rejected (sprintf "a slog: %d moves" metrics.optimal_moves)
    else if metrics.min_stamina_remaining > Config.gen_accept_max_stamina_margin
    then
      Rejected
        (sprintf "stamina never mattered (margin %d)" metrics.min_stamina_remaining)
    else (
      let families = count_families ~wall:g.wall ~start:g.start ~first_actions:actions in
      if families > Config.gen_accept_max_families
      then Rejected (sprintf "too many route families (%d)" families)
      else Accepted { metrics; families; actions })
;;

open! Core
open Types

type action =
  | Move of limb * int
  | Rest
  | Chalk of limb
[@@deriving sexp_of, compare, equal]

type metrics =
  { optimal_cost : int
  ; optimal_moves : int
  ; chalk_required : int
  ; min_stamina_remaining : int
  ; states_expanded : int
  ; max_queue_size : int
  ; critical_balance_turns : int
  }
[@@deriving sexp_of]

type solution =
  { actions : action list
  ; metrics : metrics
  }
[@@deriving sexp_of]

type outcome =
  | Solution of solution
  | No_route of { states_expanded : int }
  | Search_limit of { states_expanded : int }
[@@deriving sexp_of]

(* §4.12 solver state: torso and stamina BUCKETED so stamina 73 vs 74 (etc.)
   collapse into one node. [broken] is omitted until holds can actually
   break mid-search (Phase 5 revisits). *)
module Key = struct
  type t =
    { limbs : limb_positions
    ; torso_xb : int
    ; torso_yb : int
    ; stamina_b : int
    ; chalk_remaining : int
    ; lh_chalk : int
    ; rh_chalk : int
    }
  [@@deriving sexp_of, compare, hash]
end

let key_of (p : player_state) : Key.t =
  { limbs = p.limbs
  ; torso_xb = Int.of_float (p.torso.x /. Config.solver_torso_bucket)
  ; torso_yb = Int.of_float (p.torso.y /. Config.solver_torso_bucket)
  ; stamina_b = p.stamina / Config.solver_stamina_bucket
  ; chalk_remaining = p.chalk.remaining
  ; lh_chalk = p.chalk.left_hand_chalk
  ; rh_chalk = p.chalk.right_hand_chalk
  }
;;

(* Dominance pruning: a state with more of every resource (stamina, bag,
   per-hand chalk) at no more cost dominates — wasteful shuffles die here
   immediately instead of spawning near-duplicate subtrees. Deliberately
   stronger pessimism than the §4.12 bucketing alone; it can only prune,
   never invent, and the replay in [route_stats] revalidates whatever comes
   out. The ladder canary guards against over-pruning regressions.

   Dominance is only sound between states whose position genuinely matches:
   same limbs AND roughly the same torso region (a cheaper state with a
   worse-placed torso must not kill the better-placed one — that over-pruned
   sloper_gate to "no route" when this key was limbs-only). The dominance
   region is COARSER than the search key's bucket (x3): fine enough to keep
   high-vs-low torso routes apart, coarse enough to collapse the
   near-identical variants that otherwise blow up the frontier count. *)
module Body_key = struct
  type t =
    { limbs : limb_positions
    ; torso_xr : int
    ; torso_yr : int
    }
  [@@deriving sexp_of, compare, hash]
end

let dominance_region_scale = 3

let body_key_of (k : Key.t) : Body_key.t =
  { limbs = k.limbs
  ; torso_xr = k.torso_xb / dominance_region_scale
  ; torso_yr = k.torso_yb / dominance_region_scale
  }
;;

(* Pareto frontier per limb configuration over (stamina bucket, bag,
   per-hand chalk; g): more of every resource at no more cost dominates —
   chalk is monotone-good (it only ever lowers grip costs). *)
type frontier_entry =
  { stamina_b : int
  ; bag : int
  ; lh : int
  ; rh : int
  ; g : int
  }

let entry_of (k : Key.t) ~g =
  { stamina_b = k.stamina_b; bag = k.chalk_remaining; lh = k.lh_chalk; rh = k.rh_chalk; g }
;;

let dominates a b =
  a.stamina_b >= b.stamina_b && a.bag >= b.bag && a.lh >= b.lh && a.rh >= b.rh && a.g <= b.g
;;

let dominated frontier entry = List.exists frontier ~f:(fun e -> dominates e entry)

let frontier_add frontier entry =
  entry :: List.filter frontier ~f:(fun e -> not (dominates entry e))
;;

(* On a wall with no crimps or slopers, chalk never changes any grip cost,
   so a route containing a chalk action is strictly worse than the same
   route without it — skipping chalk edges there is optimality-preserving
   and keeps chalk from multiplying the state space for nothing. *)
let chalk_matters (wall : wall) =
  Array.exists wall.holds ~f:(fun h ->
    match h.kind with
    | Crimp | Sloper -> true
    | Jug | Foothold | Rest | Crumbling | Chalk_refill | Finish -> false)
;;

(* Neighbor generation calls the gate — the solver never invents its own
   legality rules (rule 2). *)
let neighbors ~(wall : wall) ~broken ~allow_chalk (p : player_state) =
  let moves =
    List.concat_map all_of_limb ~f:(fun limb ->
      Array.to_list wall.holds
      |> List.filter_map ~f:(fun hold ->
        match Movement.attempt_move ~wall ~broken p limb hold with
        | Error _ -> None
        | Ok next -> Some (Move (limb, hold.id), next)))
  in
  let rest =
    match Movement.attempt_rest ~wall ~broken p with
    | Error _ -> []
    | Ok next -> [ Rest, next ]
  in
  let chalks =
    if not allow_chalk
    then []
    else
      List.filter_map [ Left_hand; Right_hand ] ~f:(fun limb ->
        match Movement.attempt_chalk ~wall ~broken p limb with
        | Error _ -> None
        | Ok next -> Some (Chalk limb, next))
  in
  moves @ rest @ chalks
;;

(* Edge cost (§4.12): stamina_used + 10 x chalk_used + 5 if the resulting
   pose is Critical + 1 per action. Resting's stamina gain clamps to 0 —
   the search needs non-negative edges. *)
let edge_cost ~(wall : wall) (before : player_state) (after : player_state) =
  let stamina_used = Int.max 0 (before.stamina - after.stamina) in
  let chalk_used = Int.max 0 (before.chalk.remaining - after.chalk.remaining) in
  let critical =
    match Balance.stability wall after.limbs ~torso:after.torso with
    | Critical -> Config.solver_critical_edge_weight
    | Stable | Strained | Falling -> 0
  in
  stamina_used
  + (Config.solver_chalk_edge_weight * chalk_used)
  + critical
  + Config.solver_move_edge_weight
;;

(* Admissible, consistent A* heuristic — a LOWER bound on remaining cost, so
   optimality is preserved (A* = Dijkstra + admissible heuristic; plain
   Dijkstra drowns in cheap sideways-shuffle states, see TUNING_LOG).

   Bounds used, all provable from the gate:
   - each hand not on a Finish hold needs >= max(1, ceil(dy / hand_reach))
     more moves (a move can raise a hand by at most its reach);
   - each foot must end within max_body_length of its (finish-height) hand,
     so it needs >= ceil(dy_foot / foot_reach) more moves;
   - any hand move costs >= hand_side + upkeep_stable + 2 jugs stable grip
     + edge base; any foot move >= foot_side + upkeep + grip + base. *)
let min_hand_move_cost =
  Config.hand_side + Config.upkeep_stable + (2 * Config.jug_grip)
  + Config.solver_move_edge_weight
;;

let min_foot_move_cost =
  Config.foot_side + Config.upkeep_stable + (2 * Config.jug_grip)
  + Config.solver_move_edge_weight
;;

let heuristic ~(wall : wall) (p : player_state) =
  let ceil_div dy reach = Int.of_float (Float.round_up (dy /. reach)) in
  (* A hand can gain at most hand_reach + max_hand_below_torso height in one
     move (from 40 below the torso to 110 within reach of it). *)
  let max_hand_gain = Config.hand_reach +. Config.max_hand_below_torso in
  let hand_needs id_opt =
    match Option.bind id_opt ~f:(Wall.find wall) with
    | None -> 1
    | Some { kind = Finish; _ } -> 0
    | Some hold ->
      Int.max 1 (ceil_div (wall.finish_y -. hold.position.y) max_hand_gain)
  in
  let foot_needs id_opt =
    match Option.bind id_opt ~f:(Wall.find wall) with
    | None -> 1
    | Some hold ->
      let dy = wall.finish_y -. Config.max_body_length -. hold.position.y in
      if Float.( <= ) dy 0. then 0 else ceil_div dy Config.foot_reach
  in
  let limb_bound =
    (min_hand_move_cost * (hand_needs p.limbs.left_hand + hand_needs p.limbs.right_hand))
    + (min_foot_move_cost * (foot_needs p.limbs.left_foot + foot_needs p.limbs.right_foot))
  in
  (* The torso rises at most torso_shift_factor x hand_reach per move, and a
     winning pose needs it within hand_reach of finish_y; every move costs
     at least min_foot_move_cost. max of consistent bounds is consistent. *)
  let torso_bound =
    let dy = wall.finish_y -. Config.hand_reach -. p.torso.y in
    if Float.( <= ) dy 0.
    then 0
    else
      min_foot_move_cost
      * ceil_div dy (Config.torso_shift_factor *. Config.hand_reach)
  in
  Int.max limb_bound torso_bound
;;

(* Priority queue as a map keyed by (f, insertion seq): O(log n) push/pop,
   deterministic FIFO tie-breaking, stale entries skipped on pop. *)
module Pq_key = struct
  module T = struct
    type t = int * int [@@deriving sexp_of, compare]
  end

  include T
  include Comparator.Make (T)
end

(* Replay the reconstructed route through the gate: validates it end to end
   (any failure is a solver bug) and yields the route-quality metrics. *)
let route_stats ~wall ~broken ~(start : player_state) actions =
  List.fold
    actions
    ~init:(start, start.stamina, 0, start.chalk.remaining)
    ~f:(fun (p, min_stamina, critical, min_chalk) action ->
      let next =
        match action with
        | Rest ->
          (match Movement.attempt_rest ~wall ~broken p with
           | Ok next -> next
           | Error e ->
             raise_s [%message "solver route invalid at rest" (e : reject_reason)])
        | Chalk limb ->
          (match Movement.attempt_chalk ~wall ~broken p limb with
           | Ok next -> next
           | Error e ->
             raise_s [%message "solver route invalid at chalk" (e : reject_reason)])
        | Move (limb, hold_id) ->
          (match Wall.find wall hold_id with
           | None -> raise_s [%message "solver route names a missing hold" (hold_id : int)]
           | Some hold ->
             (match Movement.attempt_move ~wall ~broken p limb hold with
              | Ok next -> next
              | Error e ->
                raise_s
                  [%message
                    "solver route invalid" (e : reject_reason) (limb : limb) (hold_id : int)]))
      in
      let critical =
        critical
        +
        match Balance.stability wall next.limbs ~torso:next.torso with
        | Critical -> 1
        | Stable | Strained | Falling -> 0
      in
      ( next
      , Int.min min_stamina next.stamina
      , critical
      , Int.min min_chalk next.chalk.remaining ))
;;

let solve ~(wall : wall) ~(start : player_state) =
  (* Crumbling holds are treated as ALREADY BROKEN: tracking per-hold wear in
     the search key would explode the space, and assuming they survive would
     be optimistic (could claim routes the real game breaks under you). The
     pessimistic stance means solver routes never depend on crumbling holds —
     by design they are decoy material (§4.13). *)
  let broken =
    Array.fold wall.holds ~init:(Set.empty (module Int)) ~f:(fun acc h ->
      match h.kind with
      | Crumbling -> Set.add acc h.id
      | Jug | Crimp | Sloper | Foothold | Rest | Chalk_refill | Finish -> acc)
  in
  (* g-cost and predecessor (parent key + action), keyed by bucketed state *)
  let dist : (Key.t, int) Hashtbl.t = Hashtbl.create (module Key) in
  let prev : (Key.t, Key.t * action) Hashtbl.t = Hashtbl.create (module Key) in
  let settled : (Key.t, unit) Hashtbl.t = Hashtbl.create (module Key) in
  let frontiers : (Body_key.t, frontier_entry list) Hashtbl.t =
    Hashtbl.create (module Body_key)
  in
  let allow_chalk = chalk_matters wall in
  let queue = ref (Map.empty (module Pq_key)) in
  let seq = ref 0 in
  let push ~g player =
    incr seq;
    queue := Map.set !queue ~key:(g + heuristic ~wall player, !seq) ~data:(g, player)
  in
  let expanded = ref 0 in
  let max_q = ref 1 in
  Hashtbl.set dist ~key:(key_of start) ~data:0;
  push ~g:0 start;
  let rec loop () =
    match Map.min_elt !queue with
    | None -> `No_route
    | Some (qk, (g, player)) ->
      queue := Map.remove !queue qk;
      let key = key_of player in
      if Hashtbl.mem settled key
      then loop ()
      else (
        match Hashtbl.find dist key with
        | Some d when d < g -> loop () (* stale queue entry *)
        | Some _ | None ->
          Hashtbl.set settled ~key ~data:();
          incr expanded;
          if !expanded > Config.solver_max_states
          then `Limit
          else if Game.won ~wall player
          then `Goal (key, g)
          else (
            List.iter (neighbors ~wall ~broken ~allow_chalk player) ~f:(fun (action, next) ->
              let next_key = key_of next in
              if not (Hashtbl.mem settled next_key)
              then (
                let next_g = g + edge_cost ~wall player next in
                let body = body_key_of next_key in
                let frontier =
                  Option.value (Hashtbl.find frontiers body) ~default:[]
                in
                let entry = entry_of next_key ~g:next_g in
                if not (dominated frontier entry)
                then (
                  Hashtbl.set frontiers ~key:body ~data:(frontier_add frontier entry);
                  Hashtbl.set dist ~key:next_key ~data:next_g;
                  Hashtbl.set prev ~key:next_key ~data:(key, action);
                  push ~g:next_g next)));
            max_q := Int.max !max_q (Map.length !queue);
            loop ()))
  in
  match loop () with
  | `No_route -> No_route { states_expanded = !expanded }
  | `Limit -> Search_limit { states_expanded = !expanded }
  | `Goal (goal_key, cost) ->
    let actions =
      let rec walk key acc =
        match Hashtbl.find prev key with
        | None -> acc
        | Some (parent, action) -> walk parent (action :: acc)
      in
      walk goal_key []
    in
    let _final, min_stamina, critical, _min_chalk =
      route_stats ~wall ~broken ~start actions
    in
    let chalk_actions =
      List.count actions ~f:(function Chalk _ -> true | Move _ | Rest -> false)
    in
    Solution
      { actions
      ; metrics =
          { optimal_cost = cost
          ; optimal_moves = List.length actions
          ; chalk_required = chalk_actions
          ; min_stamina_remaining = min_stamina
          ; states_expanded = !expanded
          ; max_queue_size = !max_q
          ; critical_balance_turns = critical
          }
      }
;;

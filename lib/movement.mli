open! Core

(** Stamina cost of one move (§4.8/§4.9): movement by limb+direction,
    stretch/cross-body penalties, pose upkeep, and per-hand grip — the last
    two computed on the POST-move pose, so leaning into Strained/Critical
    multiplies what every hold costs you. *)
type cost =
  { movement : int
  ; penalties : int (* big stretch + cross-body *)
  ; upkeep : int
  ; grip : int
  ; total : int
  }
[@@deriving sexp_of]

(** Everything attempt_move does EXCEPT the stamina-sufficiency gate: full
    pose validation, torso shift, and cost accounting. Returns the post-move
    state (stamina clamped at 0) with the cost. Used for previews/ghosts and
    for classifying "desperate" moves — a move whose [cost.total] exceeds
    current stamina means the climber's grip gives out (Game turns it into a
    fall; the solver never takes it). *)
val preview_move
  :  wall:Types.wall
  -> broken:Set.M(Int).t
  -> Types.player_state
  -> Types.limb
  -> Types.hold
  -> (Types.player_state * cost, Types.reject_reason) result

(** THE single validation gate (CLAUDE.md §4.11, rule 2). Every state
    transition — player input, solver edge expansion, generator
    route-building — must go through here (or [attempt_rest]). Never
    duplicate validation logic.

    Checks, in order: hold exists & not broken → not illegally occupied →
    limb-compatible → within reach (incl. §4.5 vertical limits) → simulate +
    shift torso → §6.4 no-teleport recheck → §4.6 spans → balance ≠ Falling →
    cost ≤ stamina. Pure: the input state is never mutated. *)
val attempt_move
  :  wall:Types.wall
  -> broken:Set.M(Int).t
  -> Types.player_state
  -> Types.limb
  -> Types.hold
  -> (Types.player_state, Types.reject_reason) result

(** Resting (§4.3): needs at least one hand on a Rest hold, at least one foot
    attached, and a Stable pose. Restores [Config.rest_recovery] stamina
    (capped at [Config.starting_stamina]) and consumes a turn. *)
val attempt_rest
  :  wall:Types.wall
  -> broken:Set.M(Int).t
  -> Types.player_state
  -> (Types.player_state, Types.reject_reason) result

(** Chalking (§4.10): bag -1, the given hand chalked for
    [Config.chalk_duration] turns. Costs a turn, no stamina. Rejected for
    feet ([Wrong_limb_for_hold]) and empty bags ([No_chalk_left]). *)
val attempt_chalk
  :  wall:Types.wall
  -> broken:Set.M(Int).t
  -> Types.player_state
  -> Types.limb
  -> (Types.player_state, Types.reject_reason) result

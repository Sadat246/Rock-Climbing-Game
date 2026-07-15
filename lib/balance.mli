open! Core

(** Balance system (§4.7) — game-friendly, not physics. Support center is the
    average of attached limb positions; stability classifies how far the torso
    has drifted from it. *)

type report =
  { support_center : Types.point
  ; balance_distance : float (* torso -> support center, world units *)
  ; horizontally_supported : bool
    (* torso.x within [min support x - margin, max support x + margin] *)
  ; stability : Types.stability
    (* Falling when: fewer than 2 supports, horizontal check fails, or
       balance_distance beyond critical_threshold *)
  }
[@@deriving sexp_of]

(** [None] when no limbs are attached. *)
val report
  :  Types.wall
  -> Types.limb_positions
  -> torso:Types.point
  -> report option

(** Just the classification; [Falling] when nothing is attached. *)
val stability : Types.wall -> Types.limb_positions -> torso:Types.point -> Types.stability

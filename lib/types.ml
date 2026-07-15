open! Core

(* Shared types (CLAUDE.md §4.2). Coordinate convention: world units
   (~pixels); x increases rightward, y increases UPWARD from the wall base
   (matches the OCaml Graphics window). *)

type limb =
  | Left_hand
  | Right_hand
  | Left_foot
  | Right_foot
[@@deriving sexp_of, compare, equal, enumerate]

type point =
  { x : float
  ; y : float
  }
[@@deriving sexp_of, compare, equal]

type hold_kind =
  | Jug
  | Crimp
  | Sloper
  | Foothold
  | Rest
  | Crumbling
  | Chalk_refill
  | Finish
[@@deriving sexp_of, compare, equal, enumerate]

type hold =
  { id : int
  ; position : point
  ; kind : hold_kind
  ; durability : int option (* Some n for crumbling holds *)
  }
[@@deriving sexp_of, compare, equal]

(* Occupancy is derived from here — never stored on holds. *)
type limb_positions =
  { left_hand : int option (* hold id *)
  ; right_hand : int option
  ; left_foot : int option
  ; right_foot : int option
  }
[@@deriving sexp_of, compare, equal]

type chalk_state =
  { remaining : int (* uses left in bag *)
  ; left_hand_chalk : int (* turns of chalk left on hand *)
  ; right_hand_chalk : int
  }
[@@deriving sexp_of, compare, equal]

type player_state =
  { limbs : limb_positions
  ; torso : point
    (* Phase 0: maintained as the average of attached limb positions.
       Phase 2 replaces this with the shift_toward model. *)
  ; stamina : int
  ; chalk : chalk_state
  ; turn : int
  }
[@@deriving sexp_of, compare, equal]

(* Balance classification (§4.7): how far the torso has drifted from the
   center of the supporting limbs. *)
type stability =
  | Stable
  | Strained
  | Critical
  | Falling
[@@deriving sexp_of, compare, equal, enumerate]

type game_status =
  | Playing
  | Won
  | Fallen of string (* human-readable reason: crucial for debugging & tuning *)
[@@deriving sexp_of, compare, equal]

type wall =
  { holds : hold array
  ; width : int (* world units *)
  ; height : int (* world units *)
  ; finish_y : float
  }
[@@deriving sexp_of]

type game_state =
  { player : player_state
  ; wall : wall
  ; broken_holds : Set.M(Int).t
  ; status : game_status
  }
[@@deriving sexp_of]

(* Typed rejection for Movement.attempt_move (CLAUDE.md §4.11). The full
   variant is declared now; Phase 0 only produces the first four. *)
type reject_reason =
  | Hold_broken
  | Hold_occupied
  | Wrong_limb_for_hold
  | Out_of_reach
  | Span_violation
  | Would_fall
  | Insufficient_stamina
  | Needs_chalk
  | Cannot_rest (* resting needs a hand on a Rest hold, a foot attached, Stable *)
[@@deriving sexp_of, compare, equal]

open! Core

(** Hold id the given limb is attached to, if any. *)
val limb_hold_id : Types.limb_positions -> Types.limb -> int option

(** [set_limb limbs limb hold_id] reattaches [limb] to [hold_id]. *)
val set_limb : Types.limb_positions -> Types.limb -> int -> Types.limb_positions

(** All (limb, hold id) pairs for currently attached limbs. *)
val attached : Types.limb_positions -> (Types.limb * int) list

open! Core

val is_hand : Types.limb -> bool
val is_foot : Types.limb -> bool

(** Which limbs may use which hold kinds (CLAUDE.md §4.3). *)
val limb_compatible : Types.limb -> Types.hold_kind -> bool

(** Base grip cost per attached HAND, per turn (§4.3 table), before the
    balance multiplier. Feet never pay grip. *)
val grip_cost : Types.hold_kind -> chalked:bool -> int

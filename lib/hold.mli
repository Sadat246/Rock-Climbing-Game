open! Core

val is_hand : Types.limb -> bool
val is_foot : Types.limb -> bool

(** Which limbs may use which hold kinds (CLAUDE.md §4.3). *)
val limb_compatible : Types.limb -> Types.hold_kind -> bool

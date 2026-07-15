open! Core

(** Euclidean distance between two points, in world units. *)
val distance : Types.point -> Types.point -> float

(** Average of a non-empty list of points. Raises on []. *)
val average : Types.point list -> Types.point

(** Max distance from the torso at which [limb] may grab a hold (§4.5). *)
val max_reach : Types.limb -> float

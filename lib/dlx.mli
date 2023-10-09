type 'a t

val init : unit -> 'a t
val add_primary : 'a t -> 'a -> unit
val add_secondary : 'a t -> 'a -> unit
val add_shape : 'a t -> 'a list -> unit
val has_solution : 'a t -> bool
val count_solutions : 'a t -> int
val generator : 'a t -> unit -> 'a list list option

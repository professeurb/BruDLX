type 'a t

val init : unit -> 'a t
val add_primary : 'a t -> 'a -> unit
val add_secondary : 'a t -> 'a -> unit
val add_shape : 'a t -> 'a list -> unit
val compile : 'a t -> int array
val has_solution_comp : int array -> bool
val has_solution : 'a t -> bool
val count_solutions : 'a t -> int
val forward : int array -> unit
val backward : int array -> unit

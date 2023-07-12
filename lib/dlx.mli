type 'a t

val init : unit -> 'a t
val add_primary : 'a t -> 'a -> unit
val add_secondary : 'a t -> 'a -> unit
val add_shape : 'a t -> 'a list -> unit
val compile : 'a t -> int array

val compile_bigarray :
  'a t ->
  ( int64,
    Bigarray.int64_elt,
    Bigarray.c_layout )
  Bigarray.Array1.t

val has_solution_comp : int array -> bool
val has_solution : 'a t -> bool
val count_solutions : 'a t -> int

(* val count_solutions_c : 'a t -> int *)
val forward : int array -> unit
val backward : int array -> unit

val forward_c :
  ( int64,
    Bigarray.int64_elt,
    Bigarray.c_layout )
  Bigarray.Array1.t ->
  unit

val backward_c :
  ( int64,
    Bigarray.int64_elt,
    Bigarray.c_layout )
  Bigarray.Array1.t ->
  unit

val forward2_c :
  ( int64,
    Bigarray.int64_elt,
    Bigarray.c_layout )
  Bigarray.Array1.t ->
  unit

val backward2_c :
  ( int64,
    Bigarray.int64_elt,
    Bigarray.c_layout )
  Bigarray.Array1.t ->
  unit

val prepare2_c :
  ( int64,
    Bigarray.int64_elt,
    Bigarray.c_layout )
  Bigarray.Array1.t ->
  unit

external forward :
  ( int64,
    Bigarray.int64_elt,
    Bigarray.c_layout )
  Bigarray.Array1.t ->
  unit = "forward"

external backward :
  ( int64,
    Bigarray.int64_elt,
    Bigarray.c_layout )
  Bigarray.Array1.t ->
  unit = "backward"

external prepare :
  ( int64,
    Bigarray.int64_elt,
    Bigarray.c_layout )
  Bigarray.Array1.t ->
  unit = "prepare"

let left = 0
and right = 1
and up = 2
and down = 3
and data = 4
and size = 5
and long_size = 5L

let insert_horiz_big tab l i =
  let r = tab.{l + right} |> Int64.to_int in
  tab.{l + right} <- i |> Int64.of_int;
  tab.{i + left} <- l |> Int64.of_int;
  tab.{r + left} <- i |> Int64.of_int;
  tab.{i + right} <- r |> Int64.of_int

let insert_vert_big tab u i =
  let d = tab.{u + down} |> Int64.to_int in
  tab.{u + down} <- i |> Int64.of_int;
  tab.{i + up} <- u |> Int64.of_int;
  tab.{i + down} <- d |> Int64.of_int;
  tab.{d + up} <- i |> Int64.of_int

let incr_point_big incr_point i =
  incr_point.{i + data} <-
    Int64.add incr_point.{i + data} long_size

type 'a t = {
  mutable counter : int;
  item_tbl : ('a, bool * int) Hashtbl.t;
  shape_tbl : (int, 'a list * bool) Hashtbl.t;
}

let init () =
  {
    counter = size;
    item_tbl = Hashtbl.create 15;
    shape_tbl = Hashtbl.create 15;
  }

let add_primary : 'a t -> 'a -> unit =
 fun t item ->
  begin
    assert (Hashtbl.mem t.item_tbl item = false);
    Hashtbl.add t.item_tbl item (true, t.counter);
    t.counter <- size + t.counter
  end

let add_secondary : 'a t -> 'a -> unit =
 fun t item ->
  begin
    assert (Hashtbl.mem t.item_tbl item = false);
    Hashtbl.add t.item_tbl item (false, t.counter);
    t.counter <- size + t.counter
  end

let add_shape : 'a t -> 'a list -> unit =
 fun t shape ->
  begin
    List.iter
      (fun item -> assert (Hashtbl.mem t.item_tbl item))
      shape;
    Hashtbl.add t.shape_tbl t.counter (shape, true);
    t.counter <- t.counter + size;
    for _ = 2 to List.length shape do
      Hashtbl.add t.shape_tbl t.counter (shape, false);
      t.counter <- t.counter + size
    done
  end

let compile_bigarray :
    'a t ->
    ( int64,
      Bigarray.int64_elt,
      Bigarray.c_layout )
    Bigarray.Array1.t =
 fun t ->
  let arr =
    Bigarray.Array1.init Int64 Bigarray.c_layout t.counter
      (fun i -> i / size * size |> Int64.of_int)
  in
  Hashtbl.iter
    (fun _ (is_primary, pos) ->
      arr.{pos + data} <- Int64.zero;
      if is_primary then insert_horiz_big arr 0 pos)
    t.item_tbl;
  Hashtbl.iter
    (fun pos (item_list, b) ->
      if b then begin
        let curr_pos = ref pos in
        List.iter
          (fun item ->
            let _, item_pos =
              Hashtbl.find t.item_tbl item
            in
            if !curr_pos <> pos then
              insert_horiz_big arr pos !curr_pos;
            insert_vert_big arr item_pos !curr_pos;
            incr_point_big arr item_pos;
            arr.{!curr_pos + data} <-
              item_pos |> Int64.of_int;
            curr_pos := size + !curr_pos)
          item_list
      end)
    t.shape_tbl;
  arr

let has_solution pb =
  let arr = compile_bigarray pb in
  let addr = (Obj.magic arr : int64) in
  prepare arr;
  forward arr;
  not (Int64.equal arr.{down} addr)

let count_solutions pb =
  let arr = compile_bigarray pb
  and cnt = ref 0 in
  let addr = (Obj.magic arr : int64) in
  prepare arr;
  forward arr;
  while not (Int64.equal arr.{down} addr) do
    incr cnt;
    backward arr
  done;
  !cnt

let get_solution pb arr =
  let addr = (Obj.magic arr : int64) in
  let conv v = Int64.(div (sub v addr) 8L |> to_int) in
  let rec aux curr sol =
    if curr = 0 then sol
    else
      aux
        (conv arr.{curr + 1})
        (let shape, _ =
           Hashtbl.find pb.shape_tbl (conv arr.{curr + 3})
         in
         shape :: sol)
  in
  aux (conv arr.{down}) []

let generator pb =
  let arr = compile_bigarray pb in
  let addr = (Obj.magic arr : int64) in
  let conv v = Int64.(div (sub v addr) 8L |> to_int) in
  prepare arr;
  forward arr;
  fun () ->
    if conv arr.{down} = 0 then None
    else
      let sol = get_solution pb arr in
      backward arr;
      Some sol

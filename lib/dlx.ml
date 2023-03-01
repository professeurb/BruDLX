(* type 'a cmp = int array *)

let left = 0
and right = 1
and up = 2
and down = 3
and data = 4

let insert_horiz tab l i =
  let r = tab.(l + right) in
  tab.(l + right) <- i;
  tab.(i + left) <- l;
  tab.(r + left) <- i;
  tab.(i + right) <- r

let insert_vert tab u i =
  let d = tab.(u + down) in
  tab.(u + down) <- i;
  tab.(i + up) <- u;
  tab.(i + down) <- d;
  tab.(d + up) <- i

let hide_vert tab i =
  let above = tab.(i + up)
  and below = tab.(i + down) in
  assert (tab.(above + down) = i);
  assert (tab.(below + up) = i);
  tab.(above + down) <- below;
  tab.(below + up) <- above

let restore_vert tab i =
  let above = tab.(i + up)
  and below = tab.(i + down) in
  tab.(above + down) <- i;
  tab.(below + up) <- i

let incr_point tab i = tab.(i + data) <- tab.(i + data) + 1
let decr_point tab i = tab.(i + data) <- tab.(i + data) - 1

let rec cover_col_aux2 tab row pos =
  if pos <> row then begin
    hide_vert tab pos;
    decr_point tab tab.(pos + data);
    cover_col_aux2 tab row tab.(pos + right)
  end

let rec cover_col_aux1 tab col pos =
  if pos <> col then begin
    cover_col_aux2 tab pos tab.(pos + right);
    cover_col_aux1 tab col tab.(pos + down)
  end

let cover_column tab col =
  let l = tab.(col + left)
  and r = tab.(col + right) in
  tab.(l + right) <- r;
  tab.(r + left) <- l;
  cover_col_aux1 tab col tab.(col + down)

let cover_column_weird tab col =
  assert (tab.(col + down) <> col);
  let l = tab.(col + left)
  and r = tab.(col + right) in
  tab.(l + right) <- r;
  tab.(r + left) <- l;
  let prev = tab.(0 + down) in
  tab.(col + right) <- prev;
  tab.(0 + down) <- col;
  assert (tab.(col + down) <> col);
  cover_col_aux1 tab col tab.(col + down)

let rec uncover_col_aux2 tab row pos =
  if pos <> row then begin
    restore_vert tab pos;
    incr_point tab tab.(pos + data);
    uncover_col_aux2 tab row tab.(pos)
  end

let rec uncover_col_aux1 tab col pos =
  if pos <> col then begin
    uncover_col_aux2 tab pos tab.(pos + left);
    uncover_col_aux1 tab col tab.(pos + up)
  end

let rec uncover_col_aux1_weird tab col pos =
  uncover_col_aux2 tab pos tab.(pos + left);
  let pos_up = tab.(pos + up) in
  if pos_up <> col then
    uncover_col_aux1_weird tab col pos_up
  else tab.(col + down) <- pos

let uncover_column tab col =
  uncover_col_aux1 tab col tab.(col + up);
  let l = tab.(col + left) in
  let r = tab.(col + right) in
  tab.(l + right) <- col;
  tab.(r + left) <- col

let uncover_column_weird tab =
  let col = tab.(0 + down) in
  assert (col <> 0);
  tab.(0 + down) <- tab.(col + right);
  uncover_col_aux1_weird tab col tab.(col + up);
  let l = tab.(col + left) in
  let r = tab.(l + right) in
  tab.(l + right) <- col;
  tab.(r + left) <- col;
  tab.(col + right) <- r

let rec cover_right tab row pos =
  if pos <> row then begin
    cover_column tab tab.(pos + 4);
    cover_right tab row tab.(pos + 1)
  end

let rec uncover_left tab row pos =
  if pos <> row then begin
    uncover_column tab tab.(pos + 4);
    uncover_left tab row tab.(pos)
  end

(* let print_info tab = *)
(*   let rec aux1 col = *)
(*     if col <> 0 then ( *)
(*       Printf.printf " %d(%d)" col tab.(col + down); *)
(*       aux1 tab.(col + right)) *)
(*     else Printf.printf "\n" *)
(*   in *)
(*   Printf.printf "Sel :"; *)
(*   aux1 tab.(0 + down); *)
(*   Printf.printf "Row :"; *)
(*   aux1 tab.(0 + right) *)

let rec select_col tab cand card next =
  if next = 0 then cand
  else
    let new_card = tab.(next + data) in
    if new_card < card then
      select_col tab next new_card tab.(next + right)
    else select_col tab cand card tab.(next + right)

let rec forward tab =
  let col = tab.(0 + right) in
  if col <> 0 then
    let col =
      select_col tab col tab.(col + data) tab.(col + right)
    in
    let row = tab.(col + down) in
    if row = col then backward tab
    else begin
      cover_column_weird tab col;
      cover_right tab row tab.(row + right);
      forward tab
    end

and backward tab =
  let col = tab.(0 + down) in
  if col <> 0 then begin
    let row = tab.(col + down) in
    uncover_left tab row tab.(row + left);
    let next_row = tab.(row + down) in
    if next_row <> col then begin
      tab.(col + down) <- next_row;
      cover_right tab next_row tab.(next_row + right);
      forward tab
    end
    else begin
      uncover_column_weird tab;
      backward tab
    end
  end

type 'a t = {
  mutable counter : int;
  (* mutable primaries : 'a list; *)
  (* mutable secondaries : 'a list; *)
  item_tbl : ('a, bool * int) Hashtbl.t;
  shape_tbl : (int, 'a list) Hashtbl.t;
}

let init () =
  {
    counter = 5;
    item_tbl = Hashtbl.create 15;
    shape_tbl = Hashtbl.create 15;
  }

let add_primary : 'a t -> 'a -> unit =
 fun t item ->
  begin
    assert (Hashtbl.mem t.item_tbl item = false);
    Hashtbl.add t.item_tbl item (true, t.counter);
    t.counter <- 5 + t.counter
  end

let add_secondary : 'a t -> 'a -> unit =
 fun t item ->
  begin
    assert (Hashtbl.mem t.item_tbl item = false);
    Hashtbl.add t.item_tbl item (false, t.counter);
    t.counter <- 5 + t.counter
  end

let add_shape : 'a t -> 'a list -> unit =
 fun t shape ->
  begin
    List.iter
      (fun item -> assert (Hashtbl.mem t.item_tbl item))
      shape;
    Hashtbl.add t.shape_tbl t.counter shape;
    t.counter <- t.counter + (5 * List.length shape)
  end

let compile : 'a t -> int array =
 fun t ->
  let arr = Array.init t.counter (fun i -> i / 5 * 5) in
  Hashtbl.iter
    (fun _ (is_primary, pos) ->
      arr.(pos + data) <- 0;
      if is_primary then insert_horiz arr 0 pos)
    t.item_tbl;
  Hashtbl.iter
    (fun pos item_list ->
      let curr_pos = ref pos in
      List.iter
        (fun item ->
          let _, item_pos = Hashtbl.find t.item_tbl item in
          if !curr_pos <> pos then
            insert_horiz arr pos !curr_pos;
          insert_vert arr item_pos !curr_pos;
          incr_point arr item_pos;
          arr.(!curr_pos + data) <- item_pos;
          curr_pos := 5 + !curr_pos)
        item_list)
    t.shape_tbl;
  arr

let has_solution_comp arr =
  forward arr;
  arr.(0 + down) <> 0

let has_solution pb =
  let arr = compile pb in
  forward arr;
  arr.(0 + down) <> 0

let count_solutions pb =
  let arr = compile pb
  and cnt = ref 0 in
  forward arr;
  while arr.(0 + down) <> 0 do
    assert (arr.(0 + right) = 0);
    incr cnt;
    backward arr
  done;
  !cnt

let rec get_shape pb arr pos =
  match Hashtbl.find_opt pb.shape_tbl pos with
  | None -> get_shape pb arr arr.(pos + right)
  | Some s -> s

let build_solution pb arr =
  if arr.(0 + down) = 0 then None
  else begin
    let rec aux pos =
      if pos = 0 then []
      else
        get_shape pb arr arr.(pos + down)
        :: aux arr.(pos + right)
    in
    Some (aux arr.(0 + down))
  end

let first_solution pb =
  let arr = compile pb in
  forward arr;
  build_solution pb arr

let iter_solutions f pb =
  let arr = compile pb in
  forward arr;
  let rec aux () =
    match build_solution pb arr with
    | None -> ()
    | Some sol ->
        f sol;
        aux ()
  in
  aux ()

let generate_solutions pb =
  let arr = compile pb in
  let has_started = ref false
  and is_done = ref false in
  fun () ->
    if !is_done then None
    else begin
      if !has_started then backward arr
      else (
        has_started := true;
        forward arr);
      match build_solution pb arr with
      | None ->
          is_done := true;
          None
      | s -> s
    end

(* type 'a cmp = int array *)

let left = 0
and right = 1
and up = 2
and down = 3
and data = 4
and cell_size = 5

let insert_right tab l i =
  let r = tab.(l + right) in
  tab.(l + right) <- i;
  tab.(i + left) <- l;
  tab.(r + left) <- i;
  tab.(i + right) <- r

let insert_down tab u i =
  let d = tab.(u + down) in
  tab.(u + down) <- i;
  tab.(i + up) <- u;
  tab.(i + down) <- d;
  tab.(d + up) <- i

let insert_up tab d i =
  let u = tab.(d + up) in
  tab.(u + down) <- i;
  tab.(i + up) <- u;
  tab.(i + down) <- d;
  tab.(d + up) <- i

let hide_horiz tab i =
  let l = tab.(i + left)
  and r = tab.(i + right) in
  assert (tab.(l + right) = i);
  assert (tab.(r + left) = i);
  tab.(l + right) <- r;
  tab.(r + left) <- l

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

let incr_point tab pos =
  tab.(pos + data) <- tab.(tab.(pos + data) + down);
  assert (tab.(pos + data) <> 0);
  if tab.(pos + right) <> pos then begin
    hide_horiz tab pos;
    insert_right tab tab.(pos + data) pos
  end

let decr_point tab pos =
  assert (tab.(pos + data) <> 0);
  tab.(pos + data) <- tab.(tab.(pos + data) + up);
  if tab.(pos + right) <> pos then begin
    hide_horiz tab pos;
    insert_right tab tab.(pos + data) pos
  end

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
  tab.(col + right) <- col;
  cover_col_aux1 tab col tab.(col + down)

let select_column tab col =
  let l = tab.(col + left)
  and r = tab.(col + right) in
  tab.(l + right) <- r;
  tab.(r + left) <- l;
  let prev = tab.(0 + data) in
  tab.(col + right) <- prev;
  tab.(0 + data) <- col;
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

let rec deselect_col_aux1 tab col pos =
  uncover_col_aux2 tab pos tab.(pos + left);
  let pos_up = tab.(pos + up) in
  if pos_up <> col then deselect_col_aux1 tab col pos_up
  else tab.(col + down) <- pos

let uncover_column tab col =
  uncover_col_aux1 tab col tab.(col + up);
  (* let l = tab.(col + left) in *)
  (* let r = tab.(col + right) in *)
  (* tab.(l + right) <- col; *)
  (* tab.(r + left) <- col *)
  assert (tab.(col + right) = col);
  insert_right tab tab.(col + data) col

let deselect_column tab =
  let col = tab.(0 + data) in
  assert (col <> 0);
  assert (tab.(col + right) <> col);
  tab.(0 + data) <- tab.(col + right);
  assert (tab.(col + up) <> col);
  deselect_col_aux1 tab col tab.(col + up);
  (* let l = tab.(col + left) in *)
  (* let r = tab.(l + right) in *)
  (* tab.(l + right) <- col; *)
  (* tab.(r + left) <- col; *)
  (* tab.(col + right) <- r *)
  insert_right tab tab.(col + data) col

let rec cover_right tab row pos =
  if pos <> row then begin
    cover_column tab tab.(pos + data);
    cover_right tab row tab.(pos + right)
  end

let rec uncover_left tab row pos =
  if pos <> row then begin
    uncover_column tab tab.(pos + data);
    uncover_left tab row tab.(pos + left)
  end

(* let print_info tab = *)
(*   let rec aux1 col = *)
(*     if col <> 0 then ( *)
(*       Printf.printf " %d(%d)" col tab.(col + down); *)
(*       aux1 tab.(col + right)) *)
(*     else Printf.printf "\n" *)
(*   in *)
(*   Printf.printf "Sel :"; *)
(*   aux1 tab.(0 + data); *)
(*   Printf.printf "Row :"; *)
(*   aux1 tab.(0 + right) *)

(* let print_array arr = *)
(*   let n = Array.length arr / cell_size in *)
(*   for i = 0 to n - 1 do *)
(*     Printf.printf "%4d: %4d %4d %4d %4d %4d\n" (5 * i) *)
(*       arr.(5 * i) *)
(*       arr.((5 * i) + 1) *)
(*       arr.((5 * i) + 2) *)
(*       arr.((5 * i) + 3) *)
(*       arr.((5 * i) + 4) *)
(*   done *)

let choose_col tab =
  let rec aux pos =
    let cand = tab.(pos + right) in
    if cand <> pos then Some cand
    else
      let next = tab.(pos + down) in
      if next = 0 then None else aux next
  in
  aux 0

let rec forward tab =
  match choose_col tab with
  | None -> ()
  | Some col ->
      (* Printf.printf "Selecting %d\n" col; *)
      let row = tab.(col + down) in
      if row = col then backward tab
      else begin
        select_column tab col;
        cover_right tab row tab.(row + right);
        forward tab
      end

and backward tab =
  let col = tab.(0 + data) in
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
      (* Printf.printf "Deselecting %d\n" col; *)
      deselect_column tab;
      backward tab
    end
  end

type 'a t = {
  mutable size : int;
  mutable items : ('a * bool) list;
  mutable shapes : 'a list list;
}

type 'a compiled = {
  arr : int array;
      (* col_tbl : ('a, int) Hashtbl.t; *)
      (* row_tbl : (int, 'a list) Hashtbl.t; *)
}

let init () = { size = 0; items = []; shapes = [] }

let add_primary t item =
  t.items <- (item, true) :: t.items;
  t.size <- 1 + t.size

let add_secondary t item =
  t.items <- (item, false) :: t.items;
  t.size <- 1 + t.size

let add_shape t shape =
  t.shapes <- shape :: t.shapes;
  t.size <- t.size + List.length shape

let compile t =
  (* Compute maximal cardinal *)
  let card_tbl = Hashtbl.create 10 in
  List.iter
    (fun (item, _) ->
      assert (Hashtbl.mem card_tbl item = false);
      Hashtbl.add card_tbl item (ref 0))
    t.items;
  List.iter
    (fun items ->
      List.iter
        (fun item -> incr (Hashtbl.find card_tbl item))
        items)
    t.shapes;
  let max_card =
    Hashtbl.fold (fun _ cr mc -> max !cr mc) card_tbl 0
  in
  (* Printf.printf "max_card : %d\n" max_card; *)
  (* Allocate correctly-sized array *)
  let arr =
    Array.init
      ((t.size + 1 + max_card) * cell_size)
      (fun i -> i / cell_size * cell_size)
  in
  let col_tbl = Hashtbl.create 10
  and row_tbl = Hashtbl.create 10 in
  let pos = ref cell_size in
  for _ = 1 to max_card do
    insert_up arr 0 !pos;
    pos := cell_size + !pos
  done;
  List.iter
    (fun (item, is_primary) ->
      Hashtbl.add col_tbl item !pos;
      arr.(!pos + data) <- 0;
      if is_primary then insert_right arr 0 !pos;
      pos := cell_size + !pos)
    t.items;
  List.iter
    (fun item_list ->
      Hashtbl.add row_tbl !pos item_list;
      let curr_pos = ref !pos in
      List.iter
        (fun item ->
          let item_pos = Hashtbl.find col_tbl item in
          if !curr_pos <> !pos then
            insert_right arr !pos !curr_pos;
          insert_down arr item_pos !curr_pos;
          incr_point arr item_pos;
          arr.(!curr_pos + data) <- item_pos;
          curr_pos := cell_size + !curr_pos)
        item_list;
      pos := !curr_pos)
    t.shapes;
  { arr (* col_tbl;  row_tbl *) }

(* let has_solution_comp arr = *)
(*   forward arr; *)
(*   arr.(0 + data) <> 0 *)

let has_solution pb =
  let cmp = compile pb in
  forward cmp.arr;
  cmp.arr.(0 + data) <> 0

let count_solutions pb =
  let cmp = compile pb
  and cnt = ref 0 in
  forward cmp.arr;
  while cmp.arr.(0 + data) <> 0 do
    incr cnt;
    backward cmp.arr
  done;
  !cnt

(* let rec get_shape cmp pos = *)
(*   match Hashtbl.find_opt cmp.row_tbl pos with *)
(*   | None -> get_shape cmp cmp.arr.(pos + right) *)
(*   | Some shape -> shape *)

(*
let build_solution pb arr =
  if arr.(0 + data) = 0 then None
  else begin
    let rec aux pos =
      if pos = 0 then []
      else
        get_shape pb arr arr.(pos + down)
        :: aux arr.(pos + right)
    in
    Some (aux arr.(0 + data))
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

let solution_dispenser pb =
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
    *)

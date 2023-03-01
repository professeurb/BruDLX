open Brudlx

(* let pre_triangle n = *)
(*   let dlx = Dlx.init () in *)
(*   for y = 0 to n - 1 do *)
(*     for x = 0 to y do *)
(*       Dlx.add_primary dlx (x, y) *)
(*     done *)
(*   done; *)
(*   for y = 0 to n - 2 do *)
(*     for x = 0 to y do *)
(*       Dlx.add_shape dlx *)
(*         [ (x, y); (x, y + 1); (x + 1, y + 1) ] *)
(*     done *)
(*   done; *)
(*   for y = 1 to n - 2 do *)
(*     for x = 0 to y - 1 do *)
(*       Dlx.add_shape dlx *)
(*         [ (x, y); (x + 1, y); (x + 1, y + 1) ] *)
(*     done *)
(*   done; *)
(*   Dlx.compile dlx *)
(* (* Dlx.has_solution dlx *) *)

(* let triangle n = *)
(*   let dlx = Dlx.init () in *)
(*   for y = 0 to n - 1 do *)
(*     for x = 0 to y do *)
(*       Dlx.add_primary dlx (x, y) *)
(*     done *)
(*   done; *)
(*   for y = 0 to n - 2 do *)
(*     for x = 0 to y do *)
(*       Dlx.add_shape dlx *)
(*         [ (x, y); (x, y + 1); (x + 1, y + 1) ] *)
(*     done *)
(*   done; *)
(*   for y = 1 to n - 2 do *)
(*     for x = 0 to y - 1 do *)
(*       Dlx.add_shape dlx *)
(*         [ (x, y); (x + 1, y); (x + 1, y + 1) ] *)
(*     done *)
(*   done; *)
(*   let _ = Dlx.compile dlx in *)
(*   Dlx.first_solution dlx *)
(* ;; *)

(* for i = 1 to 12 do *)
(*   if i mod 3 <> 1 then *)
(*     Printf.printf "%2d: %c\n" i *)
(*       (if triangle i then '+' else '.') *)
(* done *)

let langford n =
  let dlx = Dlx.init () in
  for i = 1 to n do
    Dlx.add_primary dlx (0, i)
  done;
  for i = 1 to 2 * n do
    Dlx.add_secondary dlx (1, i)
  done;
  for i = 1 to n do
    for j = 1 to (2 * n) - 1 - i do
      Dlx.add_shape dlx [ (0, i); (1, j); (1, j + i + 1) ]
    done
  done;
  dlx

let rec range a b =
  if a > b then [] else a :: range (a + 1) b

let _ =
  assert (
    List.map
      (fun x -> langford x |> Dlx.count_solutions)
      (range 1 10)
    = [ 0; 0; 2; 2; 0; 0; 52; 300; 0; 0 ])

(* let sol n = *)
(*   match langford n |> Dlx.first_solution with *)
(*   | None -> None *)
(*   | Some l -> *)
(*       let arr = Array.make (2 * n) 0 in *)
(*       List.iter *)
(*         (function *)
(*           | [ (0, v); (1, x1); (1, x2) ] -> *)
(*               assert (arr.(x1 - 1) = 0); *)
(*               assert (arr.(x2 - 1) = 0); *)
(*               arr.(x1 - 1) <- v; *)
(*               arr.(x2 - 1) <- v *)
(*           | _ -> assert false) *)
(*         l; *)
(*       Some arr *)

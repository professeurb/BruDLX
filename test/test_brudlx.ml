open Brudlx

type elt = Val of int | Pos of int


let langford n =
  let dlx = Dlx.init () in
  for i = 1 to n do
    Dlx.add_primary dlx (Val i)
  done;
  for i = 1 to 2 * n do
    Dlx.add_secondary dlx (Pos i)
  done;
  for i = 1 to n do
    for j = 1 to (2 * n) - 1 - i do
      Dlx.add_shape dlx [ Val i; Pos j; Pos (j + i + 1) ]
    done
  done;
  dlx

let rec range a b =
  if a > b then [] else a :: range (a + 1) b


assert (
  List.map
    (fun i -> langford i |> Dlx.count_solutions)
    (range 1 10)
  = [ 0; 0; 2; 2; 0; 0; 52; 300; 0; 0 ])

let rec gen_to_list g =
  match g () with
  | None -> []
  | Some v -> v :: gen_to_list g

let _ =
  let pb = Dlx.init () in
  for i = 1 to 2 do
    Dlx.add_primary pb i
  done;
  Dlx.add_shape pb [ 1 ];
  Dlx.add_shape pb [ 2 ];
  Dlx.add_shape pb [ 1; 2 ];
  assert (
    Dlx.generator pb |> gen_to_list |> List.sort compare
    = ([ [ [ 1 ]; [ 2 ] ]; [ [ 1; 2 ] ] ]
      |> List.sort compare))

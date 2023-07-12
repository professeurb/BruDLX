open Brudlx

let triangle n =
  let dlx = Dlx.init () in
  for y = 0 to n - 1 do
    for x = 0 to y do
      Dlx.add_primary dlx (x, y)
    done
  done;
  for y = 0 to n - 2 do
    for x = 0 to y do
      Dlx.add_shape dlx
        [ (x, y); (x, y + 1); (x + 1, y + 1) ]
    done
  done;
  for y = 1 to n - 2 do
    for x = 0 to y - 1 do
      Dlx.add_shape dlx
        [ (x, y); (x + 1, y); (x + 1, y + 1) ]
    done
  done;
  Dlx.count_solutions dlx
;;

for i = 1 to 21 do
  if i mod 3 <> 1 then
    Printf.printf "%2d: %d\n%!" i (triangle i)
done

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
  Dlx.count_solutions dlx

let rec range a b =
  if a > b then [] else a :: range (a + 1) b
;;

assert (
  List.map langford (range 1 10)
  = [ 0; 0; 2; 2; 0; 0; 52; 300; 0; 0 ])

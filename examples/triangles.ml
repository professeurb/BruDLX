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
  dlx

for i = 1 to 21 do
  if i mod 3 <> 1 then
    let pb = triangle i in
    Printf.printf "%2d: %d\n%!" i
      (Dlx.count_solutions pb)
done

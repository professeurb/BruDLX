open Brudlx

type elt =
  | S
  | B of int * int
  | L of int * int
  | C of int * int
  | X of int * int

let grid str =
  let pb = Dlx.init () in
  Dlx.add_primary pb S;
  for i = 0 to 8 do
    for v = 1 to 9 do
      Dlx.add_secondary pb (B (i, v));
      Dlx.add_secondary pb (L (i, v));
      Dlx.add_secondary pb (C (i, v));
      Dlx.add_primary pb (X (i, v - 1))
    done
  done;
  for x = 0 to 8 do
    for y = 0 to 8 do
      let b = (x / 3) + (3 * (y / 3)) in
      for v = 1 to 9 do
        Dlx.add_shape pb
          [ B (b, v); L (y, v); C (x, v); X (x, y) ]
      done
    done
  done;
  let start = ref [ S ] in
  for y = 0 to 8 do
    for x = 0 to 8 do
      let c = str.[(10 * y) + x] in
      if c <> '.' then (
        let b = (x / 3) + (3 * (y / 3))
        and v = Char.code c - 48 in
        assert (v >= 1);
        assert (v <= 9);
        start := B (b, v) :: !start;
        start := L (y, v) :: !start;
        start := C (x, v) :: !start;
        start := X (x, y) :: !start)
    done
  done;
  Dlx.add_shape pb !start;
  pb

let pb =
  grid
    {|.837.16..
.4......5
....8....
....9.2..
.3.2.8.4.
..8.6....
...9.....
..13.27..
7......6.|}

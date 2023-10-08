# A fast Dancing Links library

## Examples

### [Langford sequence](https://en.wikipedia.org/wiki/Langford_pairing)

```ocaml
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
```

```ocaml
for i = 1 to 20 do
    Printf.printf "%d : %d\n%!"
        i
        ((langford i |> Dlx.count_solutions) / 2)
done
```

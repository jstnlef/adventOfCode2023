module AdventOfCode2023.Common.Itertools

let rec combinations n l =
  match n, l with
  | 0, _ -> [ [] ]
  | _, [] -> []
  | k, x :: xs -> List.map ((@) [ x ]) (combinations (k - 1) xs) @ combinations k xs

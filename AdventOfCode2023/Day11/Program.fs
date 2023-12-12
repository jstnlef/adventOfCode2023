namespace Day11

open System.IO

type Galaxy = { id: int; pos: int * int }

type Image = List<Galaxy>

module Image =
  let rec comb n l =
    match n, l with
    | 0, _ -> [ [] ]
    | _, [] -> []
    | k, x :: xs -> List.map ((@) [ x ]) (comb (k - 1) xs) @ comb k xs

  let distance (ax, ay) (bx, by) = abs (bx - ax) + abs (by - ay)

  let allShortestDistances (image: Image) =
    image
    |> comb 2
    |> Seq.map (fun galaxies -> distance (galaxies[0].pos) (galaxies[1].pos))

  let parse filename : Image =
    let mutable id = 0
    let mutable galaxies = List.empty
    let lines = filename |> File.ReadLines

    for y, line in Seq.indexed lines do
      for x, c in Seq.indexed line do
        match c with
        | '#' ->
          id <- id + 1
          let galaxy = { id = id; pos = (x, y) }
          galaxies <- galaxies @ [ galaxy ]
        | _ -> ()

    galaxies

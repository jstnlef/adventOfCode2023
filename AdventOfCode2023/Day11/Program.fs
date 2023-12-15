namespace Day11

open System.IO
open AdventOfCode2023.Common

type Galaxy = int * int

type Image = List<Galaxy>

module Image =
  let distance (ax, ay) (bx, by) : int64 =
    abs ((int64 bx) - (int64 ax)) + abs ((int64 by) - (int64 ay))

  let allShortestDistances (image: Image) =
    image
    |> Itertools.combinationsOf2
    |> Seq.map (fun galaxies -> distance (galaxies[0]) (galaxies[1]))

  let parse expansionFactor filename : Image =
    let lines = filename |> File.ReadAllLines

    let verticalList =
      [| for j in 0 .. lines[0].Length - 1 -> [| for i in 0 .. lines.Length - 1 -> lines[i][j] |] |]

    let columnsToDouble =
      verticalList
      |> Array.indexed
      |> Array.filter (fun (_, l) -> l |> Array.forall (fun c -> c = '.'))
      |> Array.map fst

    let rowsToDouble =
      lines
      |> Array.indexed
      |> Array.filter (fun (_, s) -> s |> Seq.forall (fun c -> c = '.'))
      |> Array.map fst

    let mutable id = 0
    let mutable galaxies = List.empty

    for y, line in Array.indexed lines do
      for x, c in Seq.indexed line do
        match c with
        | '#' ->
          id <- id + 1

          let modifiedX =
            x
            + ((expansionFactor - 1)
               * (columnsToDouble |> (Seq.takeWhile (fun i -> i < x) >> Seq.length)))

          let modifiedY =
            y
            + ((expansionFactor - 1)
               * (rowsToDouble |> (Seq.takeWhile (fun i -> i < y) >> Seq.length)))

          galaxies <- galaxies @ [ (modifiedX, modifiedY) ]
        | _ -> ()

    galaxies

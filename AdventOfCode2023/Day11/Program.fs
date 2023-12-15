namespace Day11

open System.IO
open AdventOfCode2023.Common

type Galaxy = { id: int; pos: int64 * int64 }

type Image = List<Galaxy>

module Image =
  let distance (ax, ay) (bx, by) : int64 = abs (bx - ax) + abs (by - ay)

  let allShortestDistances (image: Image) =
    image
    |> Itertools.combinations 2
    |> Seq.map (fun galaxies -> distance (galaxies[0].pos) (galaxies[1].pos))

  let parse expansionFactor filename : Image =
    let mutable id = 0
    let mutable galaxies = List.empty
    let lines = filename |> File.ReadAllLines
    let space = array2D lines

    let verticalList arr =
      seq { for j in 0 .. Array2D.length2 arr - 1 -> [ for i in 0 .. Array2D.length1 arr - 1 -> arr[i, j] ] }

    let columnsToDouble =
      space
      |> verticalList
      |> Seq.indexed
      |> Seq.filter (fun (_, l) -> l |> List.forall (fun c -> c = '.'))
      |> Seq.map fst

    let rowsToDouble =
      lines
      |> Seq.indexed
      |> Seq.filter (fun (_, s) -> s |> Seq.forall (fun c -> c = '.'))
      |> Seq.map fst


    for y, line in Seq.indexed lines do
      for x, c in Seq.indexed line do
        match c with
        | '#' ->
          id <- id + 1

          let modifiedX =
            x
            + ((expansionFactor - 1)
               * (columnsToDouble |> Seq.takeWhile (fun i -> i < x) |> Seq.length))

          let modifiedY =
            y
            + ((expansionFactor - 1)
               * (rowsToDouble |> Seq.takeWhile (fun i -> i < y) |> Seq.length))

          let galaxy =
            { id = id
              pos = (modifiedX, modifiedY) }

          galaxies <- galaxies @ [ galaxy ]
        | _ -> ()

    galaxies

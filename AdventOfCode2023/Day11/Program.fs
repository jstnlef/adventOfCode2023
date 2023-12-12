namespace Day11

open System.IO
open AdventOfCode2023.Common

type Galaxy = { id: int; pos: int * int }

type Image = List<Galaxy>

module Image =
  let distance (ax, ay) (bx, by) = abs (bx - ax) + abs (by - ay)

  let allShortestDistances (image: Image) =
    image
    |> Itertools.combinations 2
    |> Seq.map (fun galaxies -> distance (galaxies[0].pos) (galaxies[1].pos))

  let parse filename : Image =
    let mutable id = 0
    let mutable galaxies = List.empty
    let lines = filename |> File.ReadAllLines

    let expandedSpace =
      lines
      |> Array.toList
      // Add horizontal space
      |> List.collect (fun line ->
        if line |> Seq.forall (fun c -> c = '.') then
          [ line; line ]
        else
          [ line ])

    for y, line in Seq.indexed expandedSpace do
      for x, c in Seq.indexed line do
        match c with
        | '#' ->
          id <- id + 1
          let galaxy = { id = id; pos = (x, y) }
          galaxies <- galaxies @ [ galaxy ]
        | _ -> ()

    galaxies

namespace Day16

open System.Collections.Generic
open System.IO
open System.Linq

type Contraption = char array array

module Contraption =
  let inbounds (x, y, _, _) (contraption: Contraption) =
    x >= 0 && x < contraption[0].Length && y >= 0 && y < contraption.Length

  let countEnergizedTiles start (contraption: Contraption) =
    let nextEnergizedTiles current =
      let x, y, dx, dy = current

      match contraption[y][x] with
      | '\\' -> seq { (x, y, dy, dx) }
      | '/' -> seq { (x, y, -dy, -dx) }
      | '|' ->
        if dx <> 0 then
          seq {
            (x, y, 0, 1)
            (x, y, 0, -1)
          }
        else
          seq { current }
      | '-' ->
        if dy <> 0 then
          seq {
            (x, y, 1, 0)
            (x, y, -1, 0)
          }
        else
          seq { current }
      | '.' -> seq { current }
      | _ -> failwith "Unrecognized character!"

    let seen = HashSet()
    let q = Queue([ start ])

    while q.Count > 0 do
      let x, y, dx, dy = q.Dequeue()
      let next = x + dx, y + dy, dx, dy

      if inbounds next contraption && seen.Add(next) then
        for tile in nextEnergizedTiles next do
          q.Enqueue(tile)

    seen.Select(fun (x, y, _, _) -> x, y).ToHashSet().Count

  let findMaxEnergizedTiles (contraption: Contraption) =
    let height = contraption.Length
    let width = contraption[0].Length

    seq {
      for i in 0 .. height - 1 do
        yield i, -1, 0, 1
        yield i, height, 0, -1

      for i in 0 .. width - 1 do
        yield -1, i, 1, 0
        yield width, i, 0, -1
    }
    |> Seq.map (fun start -> countEnergizedTiles start contraption)
    |> Seq.max

  let parse filename : Contraption =
    filename |> File.ReadAllLines |> Array.map _.ToCharArray()

namespace Day16

open System.Collections.Generic
open System.IO
open System.Linq

type Contraption = char array array

module Contraption =
  let inbounds (x, y, _, _) length =
    x >= 0 && x < length && y >= 0 && y < length

  let countEnergizedTiles start (contraption: Contraption) =
    let nextEnergizedTile current =
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

      if inbounds next contraption.Length && seen.Add(next) then
        for tile in nextEnergizedTile next do
          q.Enqueue(tile)

    seen.Select(fun (x, y, _, _) -> x, y).ToHashSet().Count

  let parse filename : Contraption =
    filename |> File.ReadAllLines |> Array.map _.ToCharArray()

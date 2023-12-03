namespace Day3

open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

type Part =
  { number: int
    x: int
    y: int
    length: int }

type Schematic =
  { raw: string array; parts: Part array }

module Schematic =
  let isSymbol c =
    match c with
    | '.' -> false
    | c when Char.IsDigit(c) -> false
    | _ -> true

  let isGear c =
    match c with
    | '*' -> true
    | _ -> false

  let inBounds schematic (x, y) =
    x >= 0 && y >= 0 && x < schematic.raw[0].Length && y < schematic.raw.Length

  let adjacentSpaces schematic part : (int * int) seq =
    let x = part.x
    let y = part.y
    let length = part.length

    seq {
      // Left side
      for i in y - 1 .. y + 1 do
        yield x - 1, i
      // Right side
      for i in y - 1 .. y + 1 do
        yield x + length, i
      // Top and bottom
      for i in x .. (x + length) do
        yield i, y - 1
        yield i, y + 1
    }
    |> Seq.filter (inBounds schematic)

  let hasAdjacent schematic predicate part =
    part
    |> adjacentSpaces schematic
    |> Seq.exists (fun (x, y) -> predicate (schematic.raw[y][x]))

  let allActualPartNumbers schematic =
    schematic.parts
    |> Seq.filter (hasAdjacent schematic isSymbol)
    |> Seq.map (fun p -> p.number)

  let allGearRatios schematic =
    let gears = Dictionary<int * int, int List>()

    for part in schematic.parts do
      let maybeGear =
        part
        |> adjacentSpaces schematic
        |> Seq.tryFind (fun (x, y) -> isGear (schematic.raw[y][x]))

      match maybeGear with
      | Some(g) ->
        let nums = gears.GetValueOrDefault(g, List<int>())
        nums.Add(part.number)
        gears[g] <- nums
      | None -> ()

    gears.Values |> Seq.filter (fun v -> v.Count = 2) |> Seq.map (Seq.fold (*) 1)

  let private partsRegex = Regex("\d+")

  let parse filename =
    let lines = filename |> File.ReadAllLines

    let parts =
      lines
      |> Array.map partsRegex.Matches
      |> Array.mapi (fun i matches ->
        matches
        |> Seq.map (fun m ->
          { number = Int32.Parse(m.Value)
            x = m.Index
            y = i
            length = m.Length })
        |> Seq.toArray)
      |> Array.concat

    { raw = lines; parts = parts }

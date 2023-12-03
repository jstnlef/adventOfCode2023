namespace Day3

open System
open System.IO
open System.Text.RegularExpressions

type Part =
  { number: int
    x: int
    y: int
    length: int }

type Schematic = { raw: string array; parts: Part seq }

module Schematic =
  let isSymbol c =
    match c with
    | '.' -> false
    | c when Char.IsDigit(c) -> false
    | _ -> true

  let inBounds schematic (x, y) =
    x >= 0 && x < schematic.raw[0].Length && y >= 0 && y < schematic.raw.Length

  let adjacentSpaces schematic part : (int * int) seq =
    let x = part.x
    let y = part.y
    let length = part.length

    seq {
      // Left side
      yield x - 1, y - 1
      yield x - 1, y
      yield x - 1, y + 1
      // Right side
      yield x + length, y - 1
      yield x + length, y
      yield x + length, y + 1
      // Top and bottom
      for i in x .. (x + length) do
        yield i, y - 1
        yield i, y + 1
    }
    |> Seq.filter (inBounds schematic)

  let hasAdjacentSymbol schematic part =
    part
    |> adjacentSpaces schematic
    |> Seq.exists (fun (x, y) -> isSymbol (schematic.raw[y][x]))

  let allActualPartNumbers schematic =
    schematic.parts
    |> Seq.filter (hasAdjacentSymbol schematic)
    |> Seq.map (fun p -> p.number)

  let regex = Regex("\d+")

  let parse filename =
    let lines = filename |> File.ReadAllLines

    let parts =
      lines
      |> Array.map regex.Matches
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

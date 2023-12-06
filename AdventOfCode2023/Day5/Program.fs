namespace Day5

open System
open System.IO

open System.Text.RegularExpressions
open Common.BTree

type Seed = int64

type Range(start: int64, length: int64) =
  member x.Start = start
  member x.Length = length
  override x.GetHashCode() = hash (start, length)

  override x.Equals(b) =
    match b with
    | :? Range as r -> (start, length) = (r.Start, r.Length)
    | _ -> false

  interface IComparable with
    member x.CompareTo yobj =
      match yobj with
      | :? Range as r -> start.CompareTo(r.Start)
      | :? Seed as n ->
        if n >= start && n <= (start + length) then 0
        elif n < start then -1
        else 1
      | _ -> invalidArg "yobj" "cannot compare values of different types"

type Transform = { range: Range; destination: int64 }

type RangeMap =
  { name: string
    transforms: Tree<Range, Transform> }

module RangeMap =
  let doTransform n rangeMap =
    rangeMap.transforms
    |> searchTreeBy (fun r -> (r :> IComparable).CompareTo(n))
    |> Option.map (fun range -> range.destination + (n - range.range.Start))
    |> Option.defaultValue n

type Almanac =
  { seeds: Seed Set
    maps: RangeMap array }

module Almanac =
  let findLocation almanac seed =
    almanac.maps |> Seq.fold RangeMap.doTransform seed

  let findLowestLocation almanac =
    almanac.seeds |> Seq.map (findLocation almanac) |> Seq.min

  let parseIndividualSeeds (line: string) =
    let parseLineToIn64Set (s: string) =
      s.Trim().Split [| ' ' |] |> Array.map Int64.Parse |> Set

    (line.Split [| ':' |])[1] |> parseLineToIn64Set

  let parseSeedRanges (line: string) =
    let parseLineToRanges (s: string) =
      s.Trim().Split [| ' ' |]
      |> Array.map Int64.Parse
      |> Array.chunkBySize 2
      |> Array.map (fun chunk -> Range(chunk[0], chunk[1]))

    (line.Split [| ':' |])[1] |> parseLineToRanges

  let parse seedParseFun filename =
    let mapRegex = Regex("(?<name>[\w|\-?]+) map:\n((?<range>\d+ ?)\n?)+")
    let lines = filename |> File.ReadAllText |> (fun s -> s.Split("\n\n"))

    let seeds = seedParseFun lines[0]

    let parseMap (mapText: string) =
      let m = mapRegex.Match(mapText)

      let rangeMatch =
        m.Groups["range"].Captures
        |> Seq.map (fun c -> Int64.Parse c.Value)
        |> Seq.chunkBySize 3

      let transforms =
        rangeMatch
        |> Seq.fold
          (fun tree nums ->
            let range = Range(nums[1], nums[2])
            insertNode range { range = range; destination = nums[0] } tree)
          Tree.Empty

      { name = m.Groups["name"].Value
        transforms = transforms }

    let maps = lines[1..] |> Array.map parseMap

    { seeds = seeds; maps = maps }

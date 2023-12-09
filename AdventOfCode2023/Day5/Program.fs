namespace Day5

open System
open System.IO

open System.Text.RegularExpressions

type Range = int64 * int64

type Transform = int64 * int64 * int64

type Map = Transform array

type Almanac = { seeds: Range array; maps: Map array }

module Almanac =
  let transformRange map (startRange, endRange) =
    seq {
      let mutable start = startRange

      for transformStart, transformEnd, delta in map do
        yield start, (min transformStart endRange)
        yield (max transformStart start) + delta, (min endRange transformEnd) + delta
        start <- (max start (min transformEnd endRange))

      yield start, endRange
    }
    |> Seq.filter (fun (a, b) -> a < b)

  let transform ranges map =
    ranges |> Seq.collect (transformRange map)

  let findLowestLocation (almanac: Almanac) =
    almanac.maps |> Array.fold transform almanac.seeds |> Seq.map fst |> Seq.min

  let toIndividualSeeds (nums: int64 array) = nums |> Array.map (fun n -> n, n + 1L)

  let toSeedRanges (nums: int64 array) =
    nums
    |> Array.chunkBySize 2
    |> Array.map (fun chunk -> (chunk[0], chunk[0] + chunk[1]))

  let parse seedParseFunc filename =
    let lines = filename |> File.ReadAllText |> _.Split("\n\n")

    let parsedSeedNums =
      (lines[0].Split(":")[1]).Trim().Split(" ") |> Array.map Int64.Parse

    let seeds = seedParseFunc parsedSeedNums

    let parseMap (mapText: string) =
      let mapRegex = Regex("(?<name>[\w|\-?]+) map:\n((?<range>\d+ ?)\n?)+")
      let m = mapRegex.Match(mapText)

      let rangeMatch =
        m.Groups["range"].Captures
        |> Seq.map (fun c -> Int64.Parse c.Value)
        |> Seq.chunkBySize 3

      rangeMatch
      |> Seq.map (fun nums -> (nums[1], nums[1] + nums[2], nums[0] - nums[1]))
      |> Seq.sort
      |> Seq.toArray

    let maps = lines[1..] |> Array.map parseMap
    { seeds = seeds; maps = maps }

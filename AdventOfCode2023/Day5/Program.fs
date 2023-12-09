namespace Day5

open System
open System.IO

open System.Text.RegularExpressions

type Range = int64 * int64

type Transform = int64 * int64 * int64

type Map = Transform array

type Almanac = { seeds: Range array; maps: Map array }

module Almanac =
  let transformRange (map: Map) (startRange, endRange) =
    seq {
      for transformStart, transformEnd, delta in map do
        // Completely outside the transform range before it
        yield startRange, (min transformStart endRange)
        // Completely inside the transform
        yield (max transformStart startRange) + delta, (min endRange transformEnd) + delta
    }
    |> Seq.filter (fun (a, b) -> a < b)

  let transform (ranges: Range array) (map: Map) =
    ranges |> Seq.collect (transformRange map)
  // seq {
  //   for rangeStart, rangeEnd in ranges do
  //     for transformStart, transformEnd, delta in map do
  //       // Completely outside the transform range before it
  //       yield rangeStart, (min transformStart rangeEnd)
  //       // Completely inside the transform
  //       yield (max transformStart rangeStart) + delta, (min rangeEnd transformEnd) + delta
  // // Completely outside the transform after it
  // // yield , rangeEnd
  // }
  // |> Seq.filter (fun (a, b) -> a < b)

  let findLowestLocation (almanac: Almanac) =
    almanac.maps[..0]
    |> Seq.collect (transform almanac.seeds)
    |> Seq.map fst
    |> Seq.min

  let parseSeedRanges (line: string) =
    let parseLineToRanges (s: string) =
      s.Trim().Split [| ' ' |]
      |> Array.map Int64.Parse
      |> Array.chunkBySize 2
      |> Array.map (fun chunk -> (chunk[0], chunk[0] + chunk[1]))

    (line.Split [| ':' |])[1] |> parseLineToRanges

  let parse filename =
    let lines = filename |> File.ReadAllText |> _.Split("\n\n")
    let seeds = parseSeedRanges lines[0]

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

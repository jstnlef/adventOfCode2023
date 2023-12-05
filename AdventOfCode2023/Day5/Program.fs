namespace Day5

open System
open System.IO

type Seed = int64

type Range =
  { source: int64
    destination: int64
    length: int64 }

module Range =
  let doTransform n range = n

type Almanac = { seeds: Seed Set }

module Almanac =
  let findLocation seed almanac = seed

  let findLowestLocation almanac =
    almanac.seeds |> Seq.minBy (findLocation almanac)

  let parse filename =
    let lines = filename |> File.ReadAllLines

    let parseLineToIn64Set (s: string) =
      s.Trim().Split [| ' ' |] |> Array.map Int64.Parse |> Set

    let parseSeeds (line: string) =
      (line.Split [| ':' |])[1] |> parseLineToIn64Set

    let seeds = parseSeeds lines[0]
    { seeds = seeds }

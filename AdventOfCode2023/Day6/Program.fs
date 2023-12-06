namespace Day6

open System
open System.IO
open System.Text.RegularExpressions

type Race = int64 * int64

type RaceDocument =
  { times: int64 seq
    distances: int64 seq }

module RaceDocument =
  let numWaysToBeatRecord (time, distance) =
    // Good old quadratic formula :)
    let t = float time
    let d = float distance
    let h1 = (t + Math.Sqrt(Math.Pow(t, 2) - 4.0 * d)) / 2.0
    let h2 = (t - Math.Sqrt(Math.Pow(t, 2) - 4.0 * d)) / 2.0
    int (Math.Ceiling(h1) - Math.Floor(h2) - 1.0)

  let multipliedNumOfWaysToBeatRecord doc =
    Seq.zip doc.times doc.distances |> Seq.map numWaysToBeatRecord |> Seq.fold (*) 1

  let parseOneRace filename =
    let lines = filename |> File.ReadAllLines
    let time = Regex.Replace(lines[0].Split(":")[1], @"\s", "") |> Int64.Parse
    let distance = Regex.Replace(lines[1].Split(":")[1], @"\s", "") |> Int64.Parse
    (time, distance)

  let parse filename =
    let regex = Regex("\w+:( +(?<num>\d+))+")
    let lines = filename |> File.ReadAllLines

    let times =
      regex.Match(lines[0]).Groups["num"].Captures
      |> Seq.map (fun c -> Int64.Parse(c.Value))

    let distances =
      regex.Match(lines[1]).Groups["num"].Captures
      |> Seq.map (fun c -> Int64.Parse(c.Value))

    { times = times; distances = distances }

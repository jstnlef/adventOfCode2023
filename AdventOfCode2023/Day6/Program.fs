namespace Day6

open System
open System.IO
open System.Text.RegularExpressions

type Race = int64 * int64

type RaceDocument =
  { times: int64 array
    distances: int64 array }

module RaceDocument =
  let getRace id doc : Race = doc.times[id], doc.distances[id]

  let distanceForHeldTime time heldTime = (time - heldTime) * heldTime

  let numWaysToBeatRecord (time, distance) =
    let lower =
      seq { (time / 2L) - 1L .. -1L .. 0L }
      |> Seq.takeWhile (fun heldTime -> distanceForHeldTime time heldTime > distance)

    let upper =
      seq { time / 2L .. time }
      |> Seq.takeWhile (fun heldTime -> distanceForHeldTime time heldTime > distance)

    Seq.append lower upper |> Seq.toList |> (fun l -> l.Length)

  let multipliedNumOfWaysToBeatRecord doc =
    Array.zip doc.times doc.distances
    |> Array.map numWaysToBeatRecord
    |> Array.fold (*) 1

  let parseOneRace filename =
    let regex = Regex("\w+:( +(?<num>\d+))+")
    let lines = filename |> File.ReadAllLines

    let time =
      regex.Match(lines[0]).Groups["num"].Captures
      |> (fun captures -> String.Join("", captures))
      |> Int64.Parse

    let distance =
      regex.Match(lines[1]).Groups["num"].Captures
      |> (fun captures -> String.Join("", captures))
      |> Int64.Parse

    (time, distance)


  let parse filename : RaceDocument =
    let regex = Regex("\w+:( +(?<num>\d+))+")
    let lines = filename |> File.ReadAllLines

    let times =
      regex.Match(lines[0]).Groups["num"].Captures
      |> Seq.map (fun c -> Int64.Parse(c.Value))
      |> Seq.toArray

    let distances =
      regex.Match(lines[1]).Groups["num"].Captures
      |> Seq.map (fun c -> Int64.Parse(c.Value))
      |> Seq.toArray

    { times = times; distances = distances }

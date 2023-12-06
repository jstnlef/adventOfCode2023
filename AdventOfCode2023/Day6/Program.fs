namespace Day6

open System
open System.IO
open System.Text.RegularExpressions

type Race = int * int

type RaceDocument =
  { times: int array
    distances: int array }

module RaceDocument =
  let getRace id doc : Race = doc.times[id], doc.distances[id]

  let distanceForHeldTime time heldTime = (time - heldTime) * heldTime

  let numWaysToBeatRecord (time: int, distance: int) =
    let lower =
      seq { (time / 2) - 1 .. -1 .. 0 }
      |> Seq.takeWhile (fun heldTime -> distanceForHeldTime time heldTime > distance)

    let upper =
      seq { time / 2 .. time }
      |> Seq.takeWhile (fun heldTime -> distanceForHeldTime time heldTime > distance)

    Seq.append lower upper |> Seq.toList |> (fun l -> l.Length)

  let multipliedNumOfWaysToBeatRecord doc =
    Array.zip doc.times doc.distances
    |> Array.map numWaysToBeatRecord
    |> Array.fold (*) 1

  let parse filename : RaceDocument =
    let regex = Regex("\w+:( +(?<num>\d+))+")
    let lines = filename |> File.ReadAllLines

    let times =
      regex.Match(lines[0]).Groups["num"].Captures
      |> Seq.map (fun c -> Int32.Parse(c.Value))
      |> Seq.toArray

    let distances =
      regex.Match(lines[1]).Groups["num"].Captures
      |> Seq.map (fun c -> Int32.Parse(c.Value))
      |> Seq.toArray

    { times = times; distances = distances }

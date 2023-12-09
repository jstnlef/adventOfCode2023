namespace Day9

open System
open System.IO

type History = int64 array

type OASISReport = History array

module OASISReport =
  let delta history : int64 array =
    history |> Array.windowed 2 |> Array.map (fun window -> window[1] - window[0])

  let generateDeltas (history: History) : int64 array seq =
    seq {
      let mutable d = history
      yield d

      while Array.sum d <> 0 do
        d <- delta d
        yield d
    }

  let nextPredictedValue (history: History) : int64 =
    history |> generateDeltas |> Seq.map Array.last |> Seq.sum

  let pastPredictedValue (history: History) : int64 =
    history
    |> generateDeltas
    |> Seq.rev
    |> Seq.map Array.head
    |> Seq.fold (fun acc n -> n - acc) 0

  let nextPredictedValues (report: OASISReport) = report |> Array.map nextPredictedValue

  let pastPredictedValues (report: OASISReport) = report |> Array.map pastPredictedValue

  let parse filename =
    filename
    |> File.ReadLines
    |> Seq.map (fun s -> s.Split(" ") |> Array.map Int64.Parse)
    |> Seq.toArray

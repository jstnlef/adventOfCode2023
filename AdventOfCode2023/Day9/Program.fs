namespace Day9

open System
open System.IO

type History = int64 array

type OASISReport = History array

module OASISReport =
  let delta history : int64 array =
    history |> Array.windowed 2 |> Array.map (fun window -> window[1] - window[0])

  let nextPredictedValue (history: History) : int64 =
    seq {
      let mutable d = history
      yield d

      while Array.sum d <> 0 do
        d <- delta d
        yield d

    }
    |> Seq.map Array.last
    |> Seq.sum

  let nextPredictedValues (report: OASISReport) = report |> Array.map nextPredictedValue

  let parse filename =
    filename
    |> File.ReadLines
    |> Seq.map (fun s -> s.Split(" ") |> Array.map Int64.Parse)
    |> Seq.toArray

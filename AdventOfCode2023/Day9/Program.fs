namespace Day9

open System
open System.IO

type History = int64 array

type OASISReport = History array

module OASISReport =
  let generateDeltas (history: History) : int64 array seq =
    let delta history : int64 array =
      history |> Array.windowed 2 |> Array.map (fun window -> window[1] - window[0])

    seq {
      let mutable d = history
      yield d

      while Array.sum d <> 0 do
        d <- delta d
        yield d
    }

  let next deltas : int64 = deltas |> Seq.map Array.last |> Seq.sum

  let previous deltas : int64 =
    deltas |> Seq.map Array.head |> Seq.rev |> Seq.fold (fun acc n -> n - acc) 0

  let predictedValues transform report =
    report |> Array.map (fun history -> history |> generateDeltas |> transform)

  let parse filename =
    filename
    |> File.ReadLines
    |> Seq.map (fun s -> s.Split(" ") |> Array.map Int64.Parse)
    |> Seq.toArray

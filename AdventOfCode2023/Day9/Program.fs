namespace Day9

open System
open System.IO

type History = int array
type OASISReport = History array
type Delta = int array

module OASISReport =
  let generateDeltas: (History -> Delta seq) =
    let delta nums =
      if Array.sum nums = 0 then
        None
      else
        Some(nums, nums |> Array.windowed 2 |> Array.map (fun window -> window[1] - window[0]))

    Seq.unfold delta

  let next: (Delta seq -> int) = Seq.map Array.last >> Seq.sum

  let previous: (Delta seq -> int) =
    Seq.map Array.head >> Seq.rev >> Seq.fold (fun acc n -> n - acc) 0

  let predictedValues transform = Array.map (generateDeltas >> transform)

  let parse filename : OASISReport =
    filename
    |> File.ReadLines
    |> Seq.map (fun s -> s.Split(" ") |> Array.map Int32.Parse)
    |> Seq.toArray

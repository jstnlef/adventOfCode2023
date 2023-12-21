namespace Day15

open System.IO

module Initialization =
  let hash = Seq.fold (fun current c -> (current + int c) * 17 % 256) 0

  let hashAllSteps: string array -> int = Array.sumBy hash

  let parse filename =
    filename |> File.ReadAllText |> _.Trim().Split(",")

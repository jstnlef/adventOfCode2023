namespace Day13

open System.IO
open Microsoft.FSharp.Core

type RockFormation = char array array

module RockFormations =
  let numberAboveReflection allowableDiffs (rocks: RockFormation) =
    let isReflection i : bool =
      let allPairs =
        seq { 0 .. rocks.Length - 1 }
        |> Seq.map (fun j -> Array.tryItem (i - j) rocks, Array.tryItem (i + j + 1) rocks)
        |> Seq.takeWhile (fun (a, b) -> a.IsSome && b.IsSome)

      not (Seq.isEmpty allPairs) && Seq.forall (fun (a, b) -> a = b) allPairs

    seq { 0 .. rocks.Length }
    |> Seq.tryFind isReflection
    |> Option.map (fun n -> n + 1)
    |> Option.defaultValue 0

  let findReflectionInFormation diffAmount (formation: RockFormation) =
    let rowsAbove = formation |> (numberAboveReflection diffAmount)

    let columnsToLeft =
      if rowsAbove = 0 then
        formation |> Array.transpose |> (numberAboveReflection diffAmount)
      else
        0

    (rowsAbove * 100) + columnsToLeft

  let countReflections diffAmount : (RockFormation array -> int) =
    Seq.map (findReflectionInFormation diffAmount) >> Seq.sum

  let parse filename : RockFormation array =
    filename
    |> File.ReadAllText
    |> _.Split("\n\n")
    |> Array.map (fun s -> s.Trim().Split("\n") |> Array.map _.ToCharArray())

namespace Day13

open System.IO
open Microsoft.FSharp.Core

type RockFormation = char array array

module RockFormations =
  let numberAboveReflection allowableDiffs (rocks: RockFormation) =
    let numberOfDiffs i =
      let countRowDiffs a b =
        Seq.zip a b
        |> Seq.map (fun (c1, c2) -> System.Convert.ToInt32(c1 <> c2))
        |> Seq.sum

      seq { 0 .. rocks.Length - 1 }
      |> Seq.map (fun j -> Array.tryItem (i - j - 1) rocks, Array.tryItem (i + j) rocks)
      |> Seq.takeWhile (fun (a, b) -> a.IsSome && b.IsSome)
      |> Seq.map (fun (a, b) -> countRowDiffs a.Value b.Value)
      |> Seq.sum

    seq { 1 .. rocks.Length - 1 }
    |> Seq.tryFind (fun i -> numberOfDiffs i = allowableDiffs)
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

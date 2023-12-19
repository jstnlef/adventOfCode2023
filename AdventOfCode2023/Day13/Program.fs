namespace Day13

open System.IO
open Microsoft.FSharp.Core

type RockFormation = char array array

module RockFormations =
  let numberAboveReflection allowableDiffs (formation: RockFormation) =
    let numberOfDiffsInReflectionIsAllowed i =
      let countRowDiffs a b =
        Array.zip a b
        |> Array.map (fun (c1, c2) -> System.Convert.ToInt32(c1 <> c2))
        |> Array.sum

      (seq { 0 .. formation.Length - 1 }
       |> Seq.map (fun j -> Array.tryItem (i - j - 1) formation, Array.tryItem (i + j) formation)
       |> Seq.takeWhile (fun (a, b) -> a.IsSome && b.IsSome)
       |> Seq.sumBy (fun (a, b) -> countRowDiffs a.Value b.Value)) = allowableDiffs

    seq { 1 .. formation.Length - 1 }
    |> Seq.tryFind numberOfDiffsInReflectionIsAllowed
    |> Option.defaultValue 0

  let findReflectionInFormation diffAmount formation =
    let rowsAbove = formation |> (numberAboveReflection diffAmount)

    let columnsToLeft =
      if rowsAbove = 0 then
        formation |> Array.transpose |> numberAboveReflection diffAmount
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

namespace Day13

open System.IO
open Microsoft.FSharp.Core

type RockFormation = char array array

module RockFormations =
  let numberAboveReflection requiredNumberOfDiffs (formation: RockFormation) =
    let numberOfDiffsIsSameAsRequired i =
      let countRowDiffs a b =
        Array.zip a b |> Array.sumBy (fun (c1, c2) -> System.Convert.ToInt32(c1 <> c2))

      let boundary = min (i - 1) (formation.Length - i - 1)

      let numberOfDiffs =
        seq { 0..boundary }
        |> Seq.sumBy (fun j -> countRowDiffs formation[i - j - 1] formation[i + j])

      numberOfDiffs = requiredNumberOfDiffs

    seq { 1 .. formation.Length - 1 }
    |> Seq.tryFind numberOfDiffsIsSameAsRequired
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

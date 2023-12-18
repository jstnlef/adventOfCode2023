namespace Day13

open System.IO
open Microsoft.FSharp.Core

type RockFormation = char array2d

module RockFormations =
  let numberAboveReflection (rocks: char array array) =
    let isReflection length i : bool =
      let allPairs =
        seq { 0 .. length - 1 }
        |> Seq.map (fun j -> Array.tryItem (i - j) rocks, Array.tryItem (i + j + 1) rocks)
        |> Seq.takeWhile (fun (a, b) -> a.IsSome && b.IsSome)

      not (Seq.isEmpty allPairs) && Seq.forall (fun (a, b) -> a = b) allPairs

    seq { 0 .. rocks.Length }
    |> Seq.tryFind (isReflection rocks.Length)
    |> Option.map (fun n -> n + 1)
    |> Option.defaultValue 0

  let findReflectionInFormation (formation: RockFormation) =
    let columnsToLeft =
      seq { 0 .. Array2D.length2 formation - 1 }
      |> Seq.map (fun i -> formation[*, i])
      |> Seq.toArray
      |> numberAboveReflection

    let rowsAbove =
      seq { 0 .. Array2D.length1 formation - 1 }
      |> Seq.map (fun i -> formation[i, *])
      |> Seq.toArray
      |> numberAboveReflection

    (rowsAbove * 100) + columnsToLeft

  let countReflections: (RockFormation array -> int) =
    Seq.map findReflectionInFormation >> Seq.sum

  let parse filename : RockFormation array =
    filename
    |> File.ReadAllText
    |> _.Split("\n\n")
    |> Array.map (fun s -> s.Trim().Split("\n") |> Array.map _.ToCharArray() |> array2D)

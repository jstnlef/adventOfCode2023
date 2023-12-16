namespace Day12

open System
open System.IO
open System.Text.RegularExpressions

type Springs = { springs: string; groups: int list }

type ConditionRecord = Springs array

module ConditionRecord =
  let rec numberOfArrangements (springs: string) groups =
    match groups with
    | [] -> if (Seq.contains '#' springs) then 0 else 1
    | groupHead :: restOfGroup ->
      seq { 0 .. (springs.Length - (List.sum restOfGroup) - (List.length restOfGroup) - groupHead) }
      |> Seq.takeWhile (fun i -> not (Seq.contains '#' springs[.. i - 1]))
      |> Seq.fold
        (fun result i ->
          let next = i + groupHead

          let lessThanLength = next - 1 <= springs.Length
          let noOperatingSprings = not (Seq.contains '.' springs[i .. next - 1])
          let nextNotDamaged = springs[next..next] <> "#"

          if lessThanLength && noOperatingSprings && nextNotDamaged then
            result + numberOfArrangements springs[next + 1 ..] restOfGroup
          else
            result)
        0

  let numberOfArrangementsPerRow record =
    record
    |> Seq.map (fun { springs = springs; groups = groups } -> numberOfArrangements springs groups)

  let parse filename : ConditionRecord =
    let regex = Regex("^(?<springs>[\?\.\#]+) ((?<groups>\d+),?)+$")

    let parseLine line =
      let m = regex.Match line
      let springs = m.Groups["springs"].Value

      let groups =
        m.Groups["groups"].Captures
        |> Seq.map (fun c -> Int32.Parse c.Value)
        |> Seq.toList

      { springs = springs; groups = groups }

    filename |> File.ReadLines |> Seq.map parseLine |> Seq.toArray

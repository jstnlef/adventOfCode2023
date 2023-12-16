namespace Day12

open System
open System.IO
open System.Text.RegularExpressions
open AdventOfCode2023.Common

type Springs = string * int list

type ConditionRecord = Springs array

module rec ConditionRecord =
  let countArrangements (springs: string, groups) : int64 =
    match groups with
    | [] -> if springs.Contains('#') then 0 else 1
    | groupHead :: restOfGroup ->
      seq { 0 .. springs.Length - (List.sum restOfGroup) - restOfGroup.Length - groupHead }
      |> Seq.takeWhile (fun i -> not (springs[.. i - 1].Contains('#')))
      |> Seq.filter (fun i ->
        let next = i + groupHead

        let lessThanLength = next - 1 <= springs.Length
        let noOperatingSprings = not (springs[i .. next - 1].Contains('.'))
        let nextNotDamaged = springs[next..next] <> "#"
        lessThanLength && noOperatingSprings && nextNotDamaged)
      |> Seq.fold (fun result i -> result + numberOfArrangements (springs[i + groupHead + 1 ..], restOfGroup)) 0

  let numberOfArrangements = Functools.memoize countArrangements

  let numberOfArrangementsPerRow record = record |> Seq.map numberOfArrangements

  let parse expand filename : ConditionRecord =
    let regex = Regex("^(?<springs>[\?\.\#]+) ((?<groups>\d+),?)+$")

    let parseLine line =
      let m = regex.Match line

      let springs =
        if expand then
          String.Join("?", Seq.replicate 5 m.Groups["springs"].Value)
        else
          m.Groups["springs"].Value

      let mutable groups =
        m.Groups["groups"].Captures
        |> Seq.map (fun c -> Int32.Parse c.Value)
        |> Seq.toList

      if expand then
        groups <- groups |> Seq.replicate 5 |> List.concat

      springs, groups

    filename |> File.ReadLines |> Seq.map parseLine |> Seq.toArray

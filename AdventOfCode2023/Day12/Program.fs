namespace Day12

open System
open System.IO
open System.Text.RegularExpressions

type Condition =
  | Unknown
  | Damaged
  | Operational

module Condition =
  let parse c =
    match c with
    | '?' -> Unknown
    | '.' -> Operational
    | '#' -> Damaged
    | _ -> failwith "Unknown character"

type Springs =
  { springs: Condition array
    groups: int array }

type ConditionRecord = Springs array

module ConditionRecord =
  let numberOfArrangementsPerRow record = Seq.empty

  let parse filename : ConditionRecord =
    let regex = Regex("^(?<springs>[\?\.\#]+) ((?<groups>\d+),?)+$")

    let parseLine line =
      let m = regex.Match line
      let springs = m.Groups["springs"].Value |> Seq.map Condition.parse |> Seq.toArray

      let groups =
        m.Groups["groups"].Captures
        |> Seq.map (fun c -> Int32.Parse c.Value)
        |> Seq.toArray

      { springs = springs; groups = groups }

    filename |> File.ReadLines |> Seq.map parseLine |> Seq.toArray

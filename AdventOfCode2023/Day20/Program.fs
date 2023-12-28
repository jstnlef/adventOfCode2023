namespace Day20

open System.IO
open System.Text.RegularExpressions

type Broadcaster = { outputs: string array }
type FlipFlop = { outputs: string array }
type Conjunction = { outputs: string array }

type Module =
  | Broadcaster of Broadcaster
  | FlipFlop of FlipFlop
  | Conjunction of Conjunction

type ModuleConfig = Map<string, Module>

module ModuleConfig =
  let countPulses config = 0

  let lineRegex =
    Regex("^(?<modType>broadcaster|[%|&])(?<name>\w+)* ->(?: (?<output>\w+),*)+$")

  let parse filename : ModuleConfig =
    let parseLine config line =
      let m = lineRegex.Match(line)
      let mutable name = m.Groups["name"].Value
      let outputs = m.Groups["output"].Captures |> Seq.map _.Value |> Seq.toArray

      let moduleType =
        match m.Groups["modType"].Value with
        | "broadcaster" ->
          name <- "broadcaster"
          Broadcaster { outputs = outputs }
        | "%" -> FlipFlop { outputs = outputs }
        | "&" -> Conjunction { outputs = outputs }
        | _ -> failwith "Unknown module type"

      config |> Map.add name moduleType

    filename |> File.ReadLines |> Seq.fold parseLine Map.empty

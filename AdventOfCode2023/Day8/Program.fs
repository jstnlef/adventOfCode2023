namespace Day8

open System.IO
open System.Text.RegularExpressions

type Network = Map<string, string * string>

type WastelandMap =
  { instructions: string
    network: Network }

module WastelandMap =
  let startNode = "AAA"
  let endNode = "ZZZ"

  let findAllWaysOut (map: WastelandMap) = 0

  let findWayOut (map: WastelandMap) =
    let mutable current = startNode
    let mutable steps = 0
    let mutable index = 0

    while current <> endNode do
      let instruction = map.instructions[index]
      let left, right = map.network[current]

      match instruction with
      | 'L' -> current <- left
      | 'R' -> current <- right
      | _ -> failwith "Somehow this isn't covered?"

      index <- (index + 1) % map.instructions.Length
      steps <- steps + 1

    steps

  let parse filename =
    let lineRegex = Regex("(?<node>\w+) = \((?<left>\w+), (?<right>\w+)\)")
    let lines = filename |> File.ReadAllLines

    let parseLine map line =
      let m = lineRegex.Match line
      let node = m.Groups["node"].Value
      let left = m.Groups["left"].Value
      let right = m.Groups["right"].Value
      Map.add node (left, right) map

    let network = lines[2..] |> Seq.fold parseLine Map.empty

    { instructions = lines[0]
      network = network }

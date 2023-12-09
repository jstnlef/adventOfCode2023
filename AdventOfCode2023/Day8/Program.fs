namespace Day8

open System.IO
open System.Text.RegularExpressions

type Network = Map<string, string * string>

type WastelandMap =
  { instructions: string
    network: Network }

module WastelandMap =
  let findWayOut startNode endCheck (map: WastelandMap) : int64 =
    let mutable current = startNode
    let mutable steps = 0
    let mutable index = 0

    while not (endCheck current) do
      let instruction = map.instructions[index]
      let left, right = map.network[current]

      match instruction with
      | 'L' -> current <- left
      | 'R' -> current <- right
      | _ -> failwith "Somehow this isn't covered?"

      index <- (index + 1) % map.instructions.Length
      steps <- steps + 1

    steps

  let findAllWaysOut (map: WastelandMap) =
    let rec gcd =
      function
      | 0L, (n: int64) -> n
      | m: int64, (n: int64) -> gcd ((n % m), m)

    let lcm a b = (a * b) / (gcd (a, b))

    let mutable stepsToFinish =
      map.network.Keys
      |> Seq.filter _.EndsWith("A")
      |> Seq.map (fun start -> findWayOut start (fun s -> s.EndsWith("Z")) map)

    stepsToFinish |> Seq.reduce lcm

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

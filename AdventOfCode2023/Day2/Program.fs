namespace Day2

open System
open System.IO
open System.Text.RegularExpressions

type Color =
  | Red
  | Green
  | Blue

module Colors =
  let all = [| Red; Green; Blue |]

type CubeSet = { num: int; color: Color }

type Game = { id: int; cubes: CubeSet array }

type Cubes = { red: int; green: int; blue: int }

module Cubes =
  let multiply cube = cube.red * cube.green * cube.blue

module Games =
  let maxCubesForColor color =
    match color with
    | Red -> 12
    | Green -> 13
    | Blue -> 14

  let colorCubesIsReasonable game color =
    game.cubes
    |> Array.filter (fun c -> c.color = color)
    |> Array.forall (fun c -> c.num <= maxCubesForColor color)

  let gameIsPossible game =
    Colors.all |> Array.forall (colorCubesIsReasonable game)

  let findPossibleGameIds games =
    games |> Seq.filter gameIsPossible |> Seq.map (fun game -> game.id)

  let findFewestCubesForColor game color =
    game.cubes
    |> Array.filter (fun c -> c.color = color)
    |> Array.map (fun c -> c.num)
    |> Array.max

  let findFewestCubes game =
    { red = findFewestCubesForColor game Red
      green = findFewestCubesForColor game Green
      blue = findFewestCubesForColor game Blue }

  let gameRegex =
    Regex("^Game (?<id>\d+):(?<set>( (?<num>\d+) (?<color>\w+),*)+;?)+$")

  let parse filename : Game seq =
    let parseLine line =
      let m = gameRegex.Match(line)
      let id = Int32.Parse(m.Groups["id"].Value)
      let zipped = (Seq.zip m.Groups["num"].Captures m.Groups["color"].Captures)

      let cubes =
        zipped
        |> Seq.map (fun (num, color) ->
          let colorType =
            match color.Value with
            | "red" -> Red
            | "green" -> Green
            | "blue" -> Blue
            | _ -> failwith "Unrecognized color"

          { num = Int32.Parse(num.Value)
            color = colorType })
        |> Seq.toArray

      { id = id; cubes = cubes }

    filename |> File.ReadLines |> Seq.map parseLine

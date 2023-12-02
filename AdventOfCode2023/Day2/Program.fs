namespace Day2

open System
open System.IO
open System.Text.RegularExpressions

type Color =
  | Red
  | Green
  | Blue

type CubeSet = { num: int; color: Color }

type Game = { id: int; cubes: CubeSet array }

module Games =
  let totalRedCubes = 12
  let totalGreenCubes = 13
  let totalBlueCubes = 14

  let gameRegex =
    Regex("^Game (?<id>\d+):(?<set>( (?<num>\d+) (?<color>\w+),*)+;?)+$")

  let maxCubesForColor color =
    match color with
    | Red -> totalRedCubes
    | Green -> totalGreenCubes
    | Blue -> totalBlueCubes

  let colorDiceIsReasonable game color =
    game.cubes
    |> Array.filter (fun c -> c.color = color)
    |> Array.forall (fun c -> c.num <= maxCubesForColor color)

  let gameIsPossible game =
    let allColors = [| Red; Green; Blue |]
    allColors |> Array.forall (colorDiceIsReasonable game)

  let findPossibleGames games =
    games |> Seq.filter gameIsPossible |> Seq.map (fun game -> game.id)

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

namespace Day18

open System
open System.IO
open System.Text.RegularExpressions

type Direction =
  | Up
  | Down
  | Left
  | Right

type Color = int

module Direction =
  let parse s =
    match s with
    | "U" -> Up
    | "D" -> Down
    | "L" -> Left
    | "R" -> Right
    | _ -> failwith "Unknown"

type Instruction =
  { direction: Direction
    distance: int
    color: Color }

type DigPlan = Instruction array

type Vertex = int * int

module DigPlan =
  let deltaV instruction =
    match instruction.direction with
    | Up -> 0, -1 * instruction.distance
    | Down -> 0, 1 * instruction.distance
    | Left -> -1 * instruction.distance, 0
    | Right -> 1 * instruction.distance, 0

  let runInstruction (x, y) instruction =
    let deltaX, deltaY = deltaV instruction
    x + deltaX, y + deltaY

  let toVertices plan = Array.scan runInstruction (0, 0) plan

  let findArea vertices =
    Array.pairwise vertices
    |> Array.sumBy (fun ((x1, y1), (x2, y2)) -> x1 * y2 - x2 * y1)
    |> fun x -> (abs x) / 2

  let findPerimeter vertices =
    Array.pairwise vertices
    |> Array.sumBy (fun ((x1, y1), (x2, y2)) -> abs (x2 - x1) + abs (y2 - y1))

  let dugOutArea plan : int =
    let vertices = plan |> toVertices
    let area = findArea vertices
    let perimeter = findPerimeter vertices
    area + (perimeter / 2) + 1

  let regex = Regex("^(?<dir>.) (?<dist>\d+) \(#(?<color>\w{6})\)$")

  let parse filename =
    let parseLine line =
      let m = regex.Match(line)

      { direction = m.Groups["dir"].Value |> Direction.parse
        distance = Int32.Parse(m.Groups["dist"].Value)
        color = Convert.ToInt32(m.Groups["color"].Value, 16) }

    filename |> File.ReadAllLines |> Array.map parseLine

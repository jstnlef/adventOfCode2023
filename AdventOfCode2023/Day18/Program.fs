namespace Day18

open System
open System.IO
open System.Text.RegularExpressions
open AdventOfCode2023.Common
open Functools

// For calculating the area of a shape via its vertices
// https://en.wikipedia.org/wiki/Pick%27s_theorem
// https://en.wikipedia.org/wiki/Shoelace_formula

type Direction =
  | Up
  | Down
  | Left
  | Right

module Direction =
  let parse s =
    match s with
    | "U"
    | "3" -> Up
    | "D"
    | "1" -> Down
    | "L"
    | "2" -> Left
    | "R"
    | "0" -> Right
    | _ -> failwith "Invalid string for direction"

type Instruction =
  { direction: Direction
    distance: int64 }

module DigPlan =
  let deltaV instruction =
    match instruction.direction with
    | Up -> 0L, -1L * instruction.distance
    | Down -> 0L, 1L * instruction.distance
    | Left -> -1L * instruction.distance, 0
    | Right -> 1L * instruction.distance, 0

  let runInstruction (x, y) instruction =
    let deltaX, deltaY = deltaV instruction
    x + deltaX, y + deltaY

  let toVertices = Array.scan runInstruction (0L, 0L)

  let findArea =
    Array.pairwise
    >> Array.sumBy (uncurry Vector2d.determinant)
    >> fun d -> (abs d) / 2L

  let findPerimeter = Array.pairwise >> Array.sumBy (uncurry Vector2d.length)

  let dugOutArea plan : int64 =
    let vertices = toVertices plan
    let area = findArea vertices
    let perimeter = findPerimeter vertices
    area + (perimeter / 2L) + 1L

  let parseLine (regex: Regex) (fromBase: int) line =
    let m = regex.Match(line)

    { direction = m.Groups["dir"].Value |> Direction.parse
      distance = Convert.ToInt64(m.Groups["dist"].Value, fromBase) }

  let parseLineWithBug =
    parseLine (Regex("^(?<dir>.) (?<dist>\d+) \(#(?<color>\w{6})\)$")) 10

  let parseLineCorrected =
    parseLine (Regex("^. \d+ \(#(?<dist>\w{5})(?<dir>\w)\)$")) 16

  let parse parseLineFunc =
    File.ReadAllLines >> Array.map parseLineFunc

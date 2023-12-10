namespace Day10

open System.IO

type Location =
  | Pipe_NS
  | Pipe_WE
  | Pipe_NW
  | Pipe_NE
  | Pipe_SW
  | Pipe_SE
  | Ground
  | Start

module Location =
  let convert c =
    match c with
    | '|' -> Pipe_NE
    | '-' -> Pipe_WE
    | 'L' -> Pipe_NE
    | 'J' -> Pipe_NW
    | '7' -> Pipe_SW
    | 'F' -> Pipe_SE
    | 'S' -> Start
    | _ -> Ground

type Pipes = Location array array

module Pipes =
  let distanceToFarthestPoint pipes = 0

  let parse filename : Pipes =
    filename
    |> File.ReadAllLines
    |> Array.map (fun line -> line |> Seq.map Location.convert |> Seq.toArray)

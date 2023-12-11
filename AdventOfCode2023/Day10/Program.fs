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
    | '|' -> Pipe_NS
    | '-' -> Pipe_WE
    | 'L' -> Pipe_NE
    | 'J' -> Pipe_NW
    | '7' -> Pipe_SW
    | 'F' -> Pipe_SE
    | 'S' -> Start
    | _ -> Ground

type Pipes =
  { pipes: Location array array
    start: int * int }

module Pipes =
  let getLocType (x, y) pipes = pipes.pipes[y][x]

  let neighborDeltas =
    seq {
      yield 0, 1
      yield 0, -1
      yield 1, 0
      yield -1, 0
    }

  let startingPipes pipes =
    let x, y = pipes.start

    neighborDeltas
    |> Seq.map (fun (dx, dy) -> x + dx, y + dy)
    |> Seq.filter (fun loc -> pipes |> getLocType loc <> Ground)
    |> Set

  let distanceToFarthestPoint pipes =
    let initialLocs = startingPipes pipes
    0

  let parse filename : Pipes =
    let mutable start = (0, 0)
    let mutable pipes = List.empty

    for y, line in (filename |> File.ReadLines |> Seq.indexed) do
      let row = Array.init line.Length (fun _ -> Ground)

      for x, c in (line |> Seq.indexed) do
        let loc = Location.convert c
        row[x] <- loc

        if loc = Start then
          start <- (x, y)

      pipes <- pipes @ [ row ]

    { start = start
      pipes = pipes |> List.toArray }

namespace Day10

open System.IO

type Direction =
  | North
  | South
  | West
  | East

module Direction =
  let delta dir =
    match dir with
    | North -> (0, -1)
    | South -> (0, 1)
    | West -> (-1, 0)
    | East -> (1, 0)

  let all =
    seq {
      yield North
      yield South
      yield West
      yield East
    }

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

  let updatedLocation (x, y) dir =
    let dx, dy = Direction.delta dir
    dir, (x + dx, y + dy)

  let inbounds (x, y) pipes =
    y >= 0 && y < pipes.pipes.Length && x >= 0 && x < pipes.pipes[y].Length

  let isPipeValid pipes previousDir location =
    let locType = getLocType location pipes

    match previousDir with
    | North -> Array.contains locType [| Pipe_NS; Pipe_SE; Pipe_SW |]
    | South -> Array.contains locType [| Pipe_NS; Pipe_NE; Pipe_NW |]
    | West -> Array.contains locType [| Pipe_WE; Pipe_NE; Pipe_SE |]
    | East -> Array.contains locType [| Pipe_WE; Pipe_NW; Pipe_SW |]

  let nearbyConnectedPipes pipes location =
    Direction.all
    |> Seq.map (updatedLocation location)
    |> Seq.filter (fun (previousDir, updated) -> inbounds updated pipes && isPipeValid pipes previousDir updated)
    |> Seq.toArray

  let followPipe pipes (previousDir, location) =
    let locType = getLocType location pipes

    let travelDir =
      match previousDir, locType with
      | North, Pipe_NS -> North
      | North, Pipe_SE -> East
      | North, Pipe_SW -> West
      | South, Pipe_NS -> South
      | South, Pipe_NE -> East
      | South, Pipe_NW -> West
      | West, Pipe_WE -> West
      | West, Pipe_NE -> North
      | West, Pipe_SE -> South
      | East, Pipe_WE -> East
      | East, Pipe_NW -> North
      | East, Pipe_SW -> South
      | _ -> failwith "Not covered"

    updatedLocation location travelDir

  let distanceToFarthestPoint pipes =
    let mutable distance = 1
    let mutable current = nearbyConnectedPipes pipes pipes.start

    while snd current[0] <> snd current[1] do
      current <- current |> Array.map (followPipe pipes)
      distance <- distance + 1

    distance

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

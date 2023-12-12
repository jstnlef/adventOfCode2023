namespace Day10

open System.Collections.Generic
open System.IO

type Direction =
  | N
  | S
  | W
  | E

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
      yield S, (0, 1)
      yield N, (0, -1)
      yield E, (1, 0)
      yield W, (-1, 0)
    }

  let neighbors (x, y) =
    neighborDeltas |> Seq.map (fun (dir, (dx, dy)) -> (dir, (x + dx, y + dy)))

  let inbounds (x, y) pipes =
    y >= 0 && y < pipes.pipes.Length && x >= 0 && x < pipes.pipes[y].Length

  let nextPipe dir loc pipes =
    let locType = getLocType loc pipes

    match dir with
    | N -> Array.contains locType [| Pipe_NS; Pipe_SE; Pipe_SW |]
    | S -> Array.contains locType [| Pipe_NS; Pipe_NE; Pipe_NW |]
    | W -> Array.contains locType [| Pipe_WE; Pipe_NE; Pipe_SE |]
    | E -> Array.contains locType [| Pipe_WE; Pipe_NW; Pipe_SW |]

  let nextPipes location pipes =
    neighbors location
    |> Seq.filter (fun (dir, loc) -> inbounds loc pipes && nextPipe dir loc pipes)
    |> Seq.map snd

  let distanceToFarthestPoint pipes =
    let mutable visited =
      Array.init pipes.pipes.Length (fun _ -> Array.init pipes.pipes[0].Length (fun _ -> false))

    let queue = Queue<int * int>()
    let x, y = pipes.start
    visited[y][x] <- true
    queue.Enqueue(pipes.start)
    let mutable distance = 0

    while queue.Count > 0 do
      let next = queue.Dequeue()

      let children =
        nextPipes next pipes |> Seq.filter (fun (x, y) -> not (visited[y][x]))

      for child in children do
        queue.Enqueue(child)
        let x, y = child
        visited[y][x] <- true

      distance <- distance + 1

    distance / 2

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

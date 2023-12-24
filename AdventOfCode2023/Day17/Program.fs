namespace Day17

open System
open System.Collections.Generic
open System.IO

type Node = int * int
type HeatLossMap = int array array

module HeatLossMap =
  let reconstructPath (cameFrom: Dictionary<Node, Node>) start goal =
    seq {
      let mutable current = goal

      while current <> start do
        yield current
        current <- cameFrom[current]

      yield current
    }

  let neighbors node (cameFrom: Dictionary<Node, Node>) (map: HeatLossMap) =
    let haveNotTravelled3Blocks node =
      if not (cameFrom.ContainsKey(node)) then
        true
      else
        let deltas =
          reconstructPath cameFrom (0, 0) node
          |> Seq.truncate 4
          |> Seq.pairwise
          |> Seq.map (fun ((r, c), (pr, pc)) -> r - pr, c - pc)

        let x =
          Seq.length deltas < 3
          || deltas |> Seq.pairwise |> Seq.exists (fun (a, b) -> a <> b)

        x

    let r, c = node

    seq {
      yield r - 1, c
      yield r + 1, c
      yield r, c - 1
      yield r, c + 1
    }
    |> Seq.filter (fun (r, c) ->
      r >= 0
      && r < map.Length
      && c >= 0
      && c < map[0].Length
      && haveNotTravelled3Blocks (r, c))

  let cost (r, c) (map: HeatLossMap) = map[r][c]

  let findPath start goal heuristic (map: HeatLossMap) : Node seq =
    let q = PriorityQueue()
    let cameFrom = Dictionary()
    let costSoFar = Dictionary()
    q.Enqueue(start, 0)
    cameFrom[start] <- start
    costSoFar[start] <- 0

    let mutable break' = false

    while q.Count > 0 && break' <> true do
      let current = q.Dequeue()

      if current = goal then
        break' <- true

      if break' <> true then
        map
        |> neighbors current cameFrom
        |> Seq.iter (fun neighbor ->
          let newCost = costSoFar[current] + cost neighbor map

          if (not (costSoFar.ContainsKey(neighbor))) || newCost < costSoFar[neighbor] then
            costSoFar[neighbor] <- newCost
            let priority = newCost + heuristic neighbor goal
            q.Enqueue(neighbor, priority)
            cameFrom[neighbor] <- current)

    reconstructPath cameFrom start goal

  let minimizeHeatLoss (ar: int, ac: int) (br, bc) = Math.Abs(ar - br) + Math.Abs(ac - bc)

  let findMinHeatLoss (map: HeatLossMap) =
    findPath (0, 0) (map.Length - 1, map[0].Length - 1) minimizeHeatLoss map
    |> Seq.sumBy (fun (r, c) -> map[r][c])

  let parse filename =
    filename
    |> File.ReadAllLines
    |> Array.map (fun line -> line |> Seq.map (fun c -> int c - int '0') |> Seq.toArray)

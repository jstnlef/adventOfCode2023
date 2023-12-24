namespace Day17

open System.Collections.Generic
open System.IO

type HeatLossMap = int array array

module HeatLossMap =
  let inbounds r c height width =
    r >= 0 && r < height && c >= 0 && c < width

  let neighborDirs dr dc =
    seq {
      yield 1, 0
      yield 0, 1
      yield -1, 0
      yield 0, -1
    }
    |> Seq.filter (fun d -> (dr, dc) <> d && (-dr, -dc) <> d)

  let findMinHeatLoss least most (map: HeatLossMap) =
    let height = map.Length
    let width = map[0].Length

    let queue = PriorityQueue()
    let seen = HashSet()
    queue.Enqueue((0, 0, 0, 0, 0), 0)

    let mutable break' = false
    let mutable returnHeatLoss = 0

    while queue.Count > 0 && not break' do
      let heatLoss, r, c, dr, dc = queue.Dequeue()

      if (r, c) = (height - 1, width - 1) then
        returnHeatLoss <- heatLoss
        break' <- true

      if not break' && seen.Add((r, c, dr, dc)) then
        for ndr, ndc in neighborDirs dr dc do
          let mutable a, b, h = r, c, heatLoss

          for i in 1..most do
            a <- a + ndr
            b <- b + ndc

            if inbounds a b height width then
              h <- h + map[a][b]

              if i >= least then
                queue.Enqueue((h, a, b, ndr, ndc), h)

    returnHeatLoss

  let parse filename =
    filename
    |> File.ReadAllLines
    |> Array.map (fun line -> line |> Seq.map (fun c -> int c - int '0') |> Seq.toArray)

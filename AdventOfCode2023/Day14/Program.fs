namespace Day14

open System
open System.IO

type Platform = char array array

module Platform =
  let mirror = 'O'
  let rock = '#'
  let ground = '.'

  let roll (platform: Platform) : Platform =
    let n = Array.init platform.Length (fun i -> Array.copy platform[i])

    for c in 0 .. n[0].Length - 1 do
      for _ in 1 .. n.Length - 1 do
        for r in 1 .. n.Length - 1 do
          if n[r][c] = mirror && n[r - 1][c] = ground then
            n[r][c] <- ground
            n[r - 1][c] <- mirror

    n

  let rotate (platform: Platform) : Platform =
    let n = Array.init platform.Length (fun _ -> Array.zeroCreate platform[0].Length)

    for r in 0 .. n.Length - 1 do
      for c in 0 .. n.Length - 1 do
        n[c][platform.Length - 1 - r] <- platform[r][c]

    n

  let determineLoad (platform: Platform) =
    platform
    |> Array.indexed
    |> Array.sumBy (fun (i, row) ->
      (platform.Length - i)
      * (row |> Array.sumBy (fun c -> Convert.ToInt32((c = mirror)))))

  let loadWhenMovedNorth = roll >> determineLoad

  let runCycle (platform: Platform) =
    seq { 0..3 } |> Seq.fold (fun p _ -> p |> roll |> rotate) platform

  let loadAfterNCycles cycles platform =
    let mutable m = Map.empty
    let mutable p = platform
    let mutable currentCycle = 0
    let mutable foundCycle = false

    while not foundCycle && currentCycle < cycles do
      p <-
        match m.TryFind(p) with
        | None ->
          let next = runCycle p
          m <- m.Add(p, (next, currentCycle + 1))
          next
        | Some(_, repeatStartCycle) ->
          foundCycle <- true
          let lengthOfRepeat = m.Count - repeatStartCycle + 1

          let solutionCycle =
            repeatStartCycle + ((cycles - currentCycle) % lengthOfRepeat) - 1

          m.Values |> Seq.find (fun (_, index) -> index = solutionCycle) |> fst

      currentCycle <- currentCycle + 1

    determineLoad p

  let parse filename : Platform =
    filename |> File.ReadAllLines |> Array.map _.ToCharArray()

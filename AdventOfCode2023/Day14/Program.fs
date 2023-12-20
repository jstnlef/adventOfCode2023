namespace Day14

open System
open System.IO

type Platform = char array array

module Platform =
  let mirror = 'O'
  let rock = '#'
  let ground = '.'

  let roll (platform: Platform) : Platform =
    for c in 0 .. platform[0].Length - 1 do
      for _ in 1 .. platform.Length - 1 do
        for r in 1 .. platform.Length - 1 do
          if platform[r][c] = mirror && platform[r - 1][c] = ground then
            platform[r][c] <- ground
            platform[r - 1][c] <- mirror

    platform

  let determineLoad (platform: Platform) : int =
    platform
    |> Array.indexed
    |> Array.sumBy (fun (i, row) ->
      (platform.Length - i)
      * (row |> Array.sumBy (fun c -> Convert.ToInt32((c = mirror)))))

  let loadWhenMovedNorth = roll >> determineLoad

  let parse filename : Platform =
    filename |> File.ReadAllLines |> Array.map _.ToCharArray()

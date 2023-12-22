namespace Day16

open System.IO

type Contraption = char array array

module Contraption =
  let countEnergizedTiles contraption = 0

  let parse filename : Contraption =
    filename |> File.ReadAllLines |> Array.map _.ToCharArray()

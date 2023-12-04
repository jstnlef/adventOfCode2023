namespace Day4

open System
open System.IO
open System.Text.RegularExpressions

type ScratchCard =
  { id: string
    winning: int Set
    mine: int Set }

module ScratchCard =
  let score card =
    let intersect = Set.intersect card.winning card.mine
    pown 2 (intersect.Count - 1)

type ScratchCards = ScratchCard seq

module ScratchCards =
  let private regex =
    Regex("^Card +(?<id>\d+): +(?<winning>\d+ *)+\| +(?<mine>\d+ *)+$")

  let parse filename : ScratchCards =
    let parseLine line : ScratchCard =
      let m = regex.Match line

      let winning =
        m.Groups["winning"].Captures |> Seq.map (fun c -> Int32.Parse(c.Value)) |> Set

      let mine =
        m.Groups["mine"].Captures |> Seq.map (fun c -> Int32.Parse(c.Value)) |> Set

      { id = m.Groups["id"].Value
        winning = winning
        mine = mine }

    filename |> File.ReadLines |> Seq.map parseLine

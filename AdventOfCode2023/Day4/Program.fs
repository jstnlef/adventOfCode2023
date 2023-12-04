namespace Day4

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

type ScratchCard =
  { id: int
    winning: int Set
    mine: int Set }

module ScratchCard =
  let matching card = Set.intersect card.winning card.mine

  let score card = pown 2 ((matching card).Count - 1)

type ScratchCards = ScratchCard array

module ScratchCards =
  let determineCopies scratchcards =
    let cardCounts = Dictionary<int, int>()
    scratchcards |> Seq.iter (fun c -> cardCounts[c.id] <- 1)

    for card in scratchcards do
      let matched = (ScratchCard.matching card).Count

      for i in card.id + 1 .. card.id + matched do
        cardCounts[i] <- cardCounts[i] + cardCounts[card.id]

    Seq.sum cardCounts.Values

  let private regex =
    Regex("^Card +(?<id>\d+): +(?<winning>\d+ *)+\| +(?<mine>\d+ *)+$")

  let parse filename : ScratchCards =
    let parseLine line : ScratchCard =
      let m = regex.Match line

      let winning =
        m.Groups["winning"].Captures |> Seq.map (fun c -> Int32.Parse(c.Value)) |> Set

      let mine =
        m.Groups["mine"].Captures |> Seq.map (fun c -> Int32.Parse(c.Value)) |> Set

      { id = Int32.Parse(m.Groups["id"].Value)
        winning = winning
        mine = mine }

    filename |> File.ReadLines |> Seq.map parseLine |> Seq.toArray

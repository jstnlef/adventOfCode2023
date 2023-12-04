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
  let matchingSet card = Set.intersect card.winning card.mine

  let score card = pown 2 ((matchingSet card).Count - 1)

type ScratchCards = ScratchCard array

module ScratchCards =
  let determineCopies scratchcards =
    let cardCounts = Dictionary<int, int>()

    for card in scratchcards do
      cardCounts[card.id] <- 1

    for card in scratchcards do
      for i in card.id + 1 .. card.id + (ScratchCard.matchingSet card).Count do
        cardCounts[i] <- cardCounts[i] + cardCounts[card.id]

    Seq.sum cardCounts.Values

  let private regex =
    Regex("^Card +(?<id>\d+): +(?<winning>\d+ *)+\| +(?<mine>\d+ *)+$")

  let parse filename : ScratchCards =
    let groupToSet (group: Group) =
      group.Captures |> Seq.map (fun c -> Int32.Parse(c.Value)) |> Set

    let parseLine line : ScratchCard =
      let m = regex.Match line

      { id = Int32.Parse(m.Groups["id"].Value)
        winning = groupToSet m.Groups["winning"]
        mine = groupToSet m.Groups["mine"] }

    filename |> File.ReadLines |> Seq.map parseLine |> Seq.toArray

namespace Day4

open System
open System.IO
open System.Text.RegularExpressions

type ScratchCard =
  { id: int
    winning: int Set
    mine: int Set }

module ScratchCard =
  let matchingSet card = Set.intersect card.winning card.mine

  let matchingSetCount card = (matchingSet card).Count

  let score card = pown 2 ((matchingSetCount card) - 1)

type ScratchCards = ScratchCard array

module ScratchCards =
  let determineCopies scratchcards =
    let updateCardCount sourceId (counts: Map<int, int>) copiedId =
      Map.add copiedId (counts[copiedId] + counts[sourceId]) counts

    let countCopies counts card =
      seq { card.id + 1 .. card.id + (ScratchCard.matchingSetCount card) }
      |> Seq.fold (updateCardCount card.id) counts

    let initialCardCounts =
      scratchcards |> Seq.fold (fun counts card -> Map.add card.id 1 counts) Map.empty

    scratchcards
    |> Seq.fold countCopies initialCardCounts
    |> Seq.sumBy (fun pair -> pair.Value)

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

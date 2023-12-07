namespace Day7

open System
open System.IO

type Hand = string
type Bid = int
type CamelHands = (Hand * Bid) array

type Type =
  | HighCard = 0
  | OnePair = 1
  | TwoPair = 2
  | ThreeOfAKind = 3
  | FullHouse = 4
  | FourOfAKind = 5
  | FiveOfAKind = 6

module Hand =
  let countCards (hand: Hand) =
    hand
    |> Seq.fold
      (fun counts c ->
        counts
        |> Map.change c (fun count ->
          match count with
          | Some(n) -> Some(n + 1)
          | None -> Some(1)))
      Map.empty

  let kindByCounts jokers (counts: (char * int) array) =
    match (counts[0] |> snd) with
    | count when (count + jokers) = 5 -> Type.FiveOfAKind
    | count when (count + jokers) = 4 -> Type.FourOfAKind
    | count when (count + jokers) = 3 && (counts[1] |> snd) = 2 -> Type.FullHouse
    | count when (count + jokers) = 3 -> Type.ThreeOfAKind
    | count when (count + jokers) = 2 && (counts[1] |> snd) = 2 -> Type.TwoPair
    | count when (count + jokers) = 2 -> Type.OnePair
    | _ -> Type.HighCard

  let kindWithJacks hand =
    kindByCounts 0 (countCards hand |> Map.toArray |> Array.sortByDescending snd)

  let kindWithJokers hand =
    let counts = countCards hand
    let jokerCount = counts |> Map.tryFind 'J' |> Option.defaultValue 0

    if jokerCount = 5 then
      Type.FiveOfAKind
    else
      let sortedCounts =
        counts
        |> Map.filter (fun k _ -> k <> 'J')
        |> Map.toArray
        |> Array.sortByDescending snd

      kindByCounts jokerCount sortedCounts

  let compareHighCards (cardEval: char -> int) (hand: Hand) (otherHand: Hand) =
    let compareCards (card, otherCard) =
      (cardEval card).CompareTo(cardEval otherCard)

    Seq.zip hand otherHand
    |> Seq.map compareCards
    |> Seq.tryFind (fun c -> c <> 0)
    |> Option.defaultValue 0

  let strength kindEval cardEval hand otherHand =
    let handStrength = hand |> kindEval |> int
    let otherHandStrength = otherHand |> kindEval |> int
    let typeStrength = handStrength.CompareTo(otherHandStrength)

    if typeStrength <> 0 then
      typeStrength
    else
      compareHighCards cardEval hand otherHand

  let score rank (_, bid) = bid * (rank + 1)

  let cardValue jValue card =
    match card with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> jValue
    | 'T' -> 10
    | c -> int c - int '0'

  let strengthWithJacks: (Hand -> Hand -> int) = strength kindWithJacks (cardValue 11)

  let strengthWithJokers: (Hand -> Hand -> int) =
    strength kindWithJokers (cardValue 1)

module CamelHands =
  let totalWinnings handEval (hands: CamelHands) =
    hands
    |> Array.sortWith (fun (hand, _) (otherHand, _) -> handEval hand otherHand)
    |> Array.mapi Hand.score
    |> Array.sum

  let parse filename =
    filename
    |> File.ReadLines
    |> Seq.map (fun s ->
      let split = s.Split(" ")
      split[0], split[1] |> Int32.Parse)
    |> Seq.toArray

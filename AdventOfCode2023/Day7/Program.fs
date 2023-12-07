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
    hand.ToCharArray()
    |> Seq.fold
      (fun counts c ->
        counts
        |> Map.change c (fun count ->
          match count with
          | Some(n) -> Some(n + 1)
          | None -> Some(1)))
      Map.empty

  let kindWithJacks (hand: Hand) =
    let counts = countCards hand |> Map.toArray |> Array.sortByDescending snd

    match counts[0] with
    | _, count when count = 5 -> Type.FiveOfAKind
    | _, count when count = 4 -> Type.FourOfAKind
    | _, count when count = 3 && (snd counts[1]) = 2 -> Type.FullHouse
    | _, count when count = 3 -> Type.ThreeOfAKind
    | _, count when count = 2 && (snd counts[1]) = 2 -> Type.TwoPair
    | _, count when count = 2 -> Type.OnePair
    | _ -> Type.HighCard

  let cardValueWithJacks card =
    match card with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | c -> Int32.Parse(string c)

  let kindWithJokers (hand: Hand) =
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

      match sortedCounts[0] with
      | _, count when (count + jokerCount) = 5 -> Type.FiveOfAKind
      | _, count when (count + jokerCount) = 4 -> Type.FourOfAKind
      | _, count when (count + jokerCount) = 3 && (snd sortedCounts[1]) = 2 -> Type.FullHouse
      | _, count when (count + jokerCount) = 3 -> Type.ThreeOfAKind
      | _, count when (count + jokerCount) = 2 && (snd sortedCounts[1]) = 2 -> Type.TwoPair
      | _, count when (count + jokerCount) = 2 -> Type.OnePair
      | _ -> Type.HighCard

  let cardValueWithJokers card =
    match card with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'T' -> 10
    | 'J' -> 1
    | c -> Int32.Parse(string c)

  let compareHighCards (cardEval: char -> int) (hand: Hand) (otherHand: Hand) =
    let compareCards (card, otherCard) =
      (cardEval card).CompareTo(cardEval otherCard)

    Seq.zip (hand.ToCharArray()) (otherHand.ToCharArray())
    |> Seq.tryFind (fun cards -> compareCards cards <> 0)
    |> Option.map compareCards
    |> Option.defaultValue 0

  let strength kindEval cardEval hand otherHand =
    let compareType = (hand |> kindEval |> int).CompareTo(otherHand |> kindEval |> int)

    if compareType <> 0 then
      compareType
    else
      compareHighCards cardEval hand otherHand

  let score rank (_, bid) = bid * (rank + 1)

  let strengthWithJacks: (Hand -> Hand -> int) =
    strength kindWithJacks cardValueWithJacks

  let strengthWithJokers: (Hand -> Hand -> int) =
    strength kindWithJokers cardValueWithJokers

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

namespace Day7

open System
open System.IO

type Hand = string
type Bid = int
type CamelHands = (Hand * Bid) array

type Type =
  | HighCard = 0
  | OnePair = 1000
  | TwoPair = 2000
  | ThreeOfAKind = 3000
  | FullHouse = 4000
  | FourOfAKind = 5000
  | FiveOfAKind = 6000

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
    |> Map.toArray
    |> Array.sortByDescending snd

  let kind (hand: Hand) =
    let counts = countCards hand

    match counts[0] with
    | _, count when count = 5 -> Type.FiveOfAKind
    | _, count when count = 4 -> Type.FourOfAKind
    | _, count when count = 3 && (snd counts[1]) = 2 -> Type.FullHouse
    | _, count when count = 3 -> Type.ThreeOfAKind
    | _, count when count = 2 && (snd counts[1]) = 2 -> Type.TwoPair
    | _, count when count = 2 -> Type.OnePair
    | _ -> Type.HighCard

  let compareHighCards (hand: Hand) (otherHand: Hand) =
    let cardValue card =
      match card with
      | 'A' -> 14
      | 'K' -> 13
      | 'Q' -> 12
      | 'J' -> 11
      | 'T' -> 10
      | c -> Int32.Parse(string c)

    let compareCards (card, otherCard) =
      (cardValue card).CompareTo(cardValue otherCard)

    Seq.zip (hand.ToCharArray()) (otherHand.ToCharArray())
    |> Seq.tryFind (fun (card, otherCard) -> cardValue card <> cardValue otherCard)
    |> Option.map compareCards
    |> Option.defaultValue 0

  let strength (hand: Hand, _) (otherHand: Hand, _) =
    let compareType = (hand |> kind |> int).CompareTo(otherHand |> kind |> int)

    if compareType <> 0 then
      compareType
    else
      compareHighCards hand otherHand

  let score rank (_, bid) = bid * (rank + 1)

module CamelHands =
  let totalWinnings (hands: CamelHands) =
    hands |> Array.sortWith Hand.strength |> Array.mapi Hand.score |> Array.sum

  let parse filename =
    filename
    |> File.ReadLines
    |> Seq.map (fun s ->
      let split = s.Split(" ")
      split[0], split[1] |> Int32.Parse)
    |> Seq.toArray

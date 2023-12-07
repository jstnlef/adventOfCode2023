namespace Day7

open System
open System.IO

type Hand = string
type Bid = int
type CamelHands = (Hand * Bid) array

type Type =
  | HighCard = 0
  | OnePair = 100
  | TwoPair = 200
  | ThreeOfAKind = 300
  | FullHouse = 400
  | FourOfAKind = 500
  | FiveOfAKind = 600

module Hand =
  let kind hand = Type.FiveOfAKind

  let value card =
    match card with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | c -> Int32.Parse(string c)

  let strength (hand: Hand, _) = (int (kind hand)) + (value hand[0])

  let score rank (_, bid) = bid * (rank + 1)

module CamelHands =
  let totalWinnings (hands: CamelHands) =
    hands |> Array.sortBy Hand.strength |> Array.mapi Hand.score |> Array.sum

  let parse filename =
    filename
    |> File.ReadLines
    |> Seq.map (fun s ->
      let split = s.Split(" ")
      split[0], split[1] |> Int32.Parse)
    |> Seq.toArray

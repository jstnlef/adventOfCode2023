module Day4.Tests

open Xunit

[<Theory>]
[<InlineData("Day4/testInput.txt", 13)>]
[<InlineData("Day4/input.txt", 18653)>]
let ``Total points of the winning scratchcards`` (filename: string, expected: int) =
  let result = filename |> ScratchCards.parse |> Seq.map ScratchCard.score |> Seq.sum
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day4/testInput.txt", 30)>]
[<InlineData("Day4/input.txt", 5921508)>]
let ``Total number of scratchcards after copies`` (filename: string, expected: int) =
  let result = filename |> ScratchCards.parse |> ScratchCards.determineCopies
  Assert.Equal(expected, result)

module Day7.Tests

open Xunit

[<Theory>]
[<InlineData("Day7/testInput.txt", 6440)>]
[<InlineData("Day7/input.txt", 256448566)>]
let ``The total winnings of the Camel Cards hands`` (filename: string, expected: int) =
  let result =
    filename
    |> CamelHands.parse
    |> (CamelHands.totalWinnings Hand.strengthWithJacks)

  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day7/testInput.txt", 5905)>]
[<InlineData("Day7/input.txt", 254412181)>]
let ``The total winnings of the Camel Cards hands with Jokers`` (filename: string, expected: int) =
  let result =
    filename
    |> CamelHands.parse
    |> (CamelHands.totalWinnings Hand.strengthWithJokers)

  Assert.Equal(expected, result)

module Day19.Tests

open Xunit

[<Theory>]
[<InlineData("Day19/testInput.txt", 19114)>]
[<InlineData("Day19/input.txt", 399284)>]
let ``Add together all of the rating numbers for all of the accepted parts`` (filename: string, expected: int) =
  let result = filename |> System.parse |> System.sumOfAcceptedParts
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day19/testInput.txt", -1)>]
[<InlineData("Day19/input.txt", -1)>]
let ``test 2`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)

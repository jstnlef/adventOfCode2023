module Day19.Tests

open Xunit

[<Theory>]
[<InlineData("Day19/testInput.txt", 19114)>]
[<InlineData("Day19/input.txt", 399284)>]
let ``Add together all of the rating numbers for all of the accepted parts`` (filename: string, expected: int64) =
  let result = filename |> System.parse |> System.sumOfAcceptedParts
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day19/testInput.txt", 167409079868000L)>]
[<InlineData("Day19/input.txt", 121964982771486L)>]
let ``Number of distinct combinations of parts ratings which will be accepted`` (filename: string, expected: int64) =
  let result = filename |> System.parse |> System.combinationsOfAcceptedRatings
  Assert.Equal(expected, result)

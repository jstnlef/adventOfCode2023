module Day6.Tests

open Xunit

[<Theory>]
[<InlineData("Day6/testInput.txt", 288)>]
[<InlineData("Day6/input.txt", 840336)>]
let ``Multiple of the number of ways you could beat the record in each race`` (filename: string, expected: int) =
  let result =
    filename |> RaceDocument.parse |> RaceDocument.multipliedNumOfWaysToBeatRecord

  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day6/testInput.txt", -1)>]
[<InlineData("Day6/input.txt", -1)>]
let ``test 2`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)

module Day9.Tests

open Xunit

[<Theory>]
[<InlineData("Day9/testInput.txt", 114)>]
[<InlineData("Day9/input.txt", 1930746032)>]
let ``The sum of the extrapolated values`` (filename: string, expected: int64) =
  let result =
    filename |> OASISReport.parse |> OASISReport.nextPredictedValues |> Array.sum

  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day9/testInput.txt", -1)>]
[<InlineData("Day9/input.txt", -1)>]
let ``test 2`` (filename: string, expected: int64) =
  let result = 0
  Assert.Equal(expected, result)

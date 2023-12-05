module Day5.Tests

open Xunit

[<Theory>]
[<InlineData("Day5/testInput.txt", 35)>]
[<InlineData("Day5/input.txt", -1)>]
let ``The lowest location number that corresponds to any of the initial seeds`` (filename: string, expected: int64) =
  let result = filename |> Almanac.parse |> Almanac.findLowestLocation
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day5/testInput.txt", -1)>]
[<InlineData("Day5/input.txt", -1)>]
let ``test 2`` (filename: string, expected: int64) =
  let result = 0
  Assert.Equal(expected, result)

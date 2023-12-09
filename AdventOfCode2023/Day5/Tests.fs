module Day5.Tests

open Xunit

[<Theory>]
[<InlineData("Day5/testInput.txt", 35)>]
[<InlineData("Day5/input.txt", 318728750)>]
let ``The lowest location number that corresponds to any of the initial seeds`` (filename: string, expected: int64) =
  let result =
    filename
    |> (Almanac.parse Almanac.parseIndividualSeeds)
    |> Almanac.findLowestLocation

  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day5/testInput.txt", 46)>]
[<InlineData("Day5/input.txt", 37384986)>]
let ``The lowest location number that corresponds to any of the initial seeds with seed ranges``
  (
    filename: string,
    expected: int64
  ) =

  let result =
    filename
    |> (Almanac.parse Almanac.parseSeedRanges)
    |> Almanac.findLowestLocation

  Assert.Equal(expected, result)

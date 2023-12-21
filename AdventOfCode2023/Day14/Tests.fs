module Day14.Tests

open Xunit

[<Theory>]
[<InlineData("Day14/testInput.txt", 136)>]
[<InlineData("Day14/input.txt", 106186)>]
let ``The total load on the north support beams`` (filename: string, expected: int) =
  let result = filename |> Platform.parse |> Platform.loadWhenMovedNorth
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day14/testInput.txt", 64)>]
[<InlineData("Day14/input.txt", 106390)>]
let ``The total load on the north support beams after many spin cycles`` (filename: string, expected: int) =
  let result = filename |> Platform.parse |> Platform.loadAfterNCycles 1_000_000_000
  Assert.Equal(expected, result)

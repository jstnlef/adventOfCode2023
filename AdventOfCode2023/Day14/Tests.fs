module Day14.Tests

open Xunit

[<Theory>]
[<InlineData("Day14/testInput.txt", 136)>]
[<InlineData("Day14/input.txt", 106186)>]
let ``The total load on the north support beams`` (filename: string, expected: int) =
  let result = filename |> Platform.parse |> Platform.loadWhenMovedNorth
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day14/testInput.txt", -1)>]
[<InlineData("Day14/input.txt", -1)>]
let ``test 2`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)

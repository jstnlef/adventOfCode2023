module Day12.Tests

open Xunit

[<Theory>]
[<InlineData("Day12/testInput.txt", -1)>]
[<InlineData("Day12/input.txt", -1)>]
let ``test`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day12/testInput.txt", -1)>]
[<InlineData("Day12/input.txt", -1)>]
let ``test 2`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)

module Day15.Tests

open Xunit

[<Theory>]
[<InlineData("Day15/testInput.txt", -1)>]
[<InlineData("Day15/input.txt", -1)>]
let ``test`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day15/testInput.txt", -1)>]
[<InlineData("Day15/input.txt", -1)>]
let ``test 2`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)

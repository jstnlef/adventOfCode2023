module Day7.Tests

open Xunit

[<Theory>]
[<InlineData("Day7/testInput.txt", -1)>]
[<InlineData("Day7/input.txt", -1)>]
let ``test`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day7/testInput.txt", -1)>]
[<InlineData("Day7/input.txt", -1)>]
let ``test 2`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)

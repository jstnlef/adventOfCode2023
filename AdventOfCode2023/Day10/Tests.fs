module Day10.Tests

open Xunit

[<Theory>]
[<InlineData("Day10/testInput.txt", -1)>]
[<InlineData("Day10/input.txt", -1)>]
let ``test`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day10/testInput.txt", -1)>]
[<InlineData("Day10/input.txt", -1)>]
let ``test 2`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)

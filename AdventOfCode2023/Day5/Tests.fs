module Day5.Tests

open Xunit

[<Theory>]
[<InlineData("Day5/testInput.txt", -1)>]
[<InlineData("Day5/input.txt", -1)>]
let ``test`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day5/testInput.txt", -1)>]
[<InlineData("Day5/input.txt", -1)>]
let ``test 2`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)

module Day3.Tests

open Xunit

[<Theory>]
[<InlineData("Day3/testInput.txt", 0)>]
[<InlineData("Day3/input.txt", 0)>]
let ``test`` (filename: string, expected: int) =
  let result = -1
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day3/testInput.txt", 0)>]
[<InlineData("Day3/input.txt", 0)>]
let ``test 2`` (filename: string, expected: int) =
  let result = -1
  Assert.Equal(expected, result)
module Day2.Tests

open Xunit

[<Theory>]
[<InlineData("Day2/testInput.txt", 0)>]
[<InlineData("Day2/input.txt", 0)>]
let ``test`` (filename: string, expected: int) =
  let result = -1
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day2/testInput.txt", 0)>]
[<InlineData("Day2/input.txt", 0)>]
let ``test 2`` (filename: string, expected: int) =
  let result = -1
  Assert.Equal(expected, result)

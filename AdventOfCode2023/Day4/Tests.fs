module Day4.Tests

open Xunit

[<Theory>]
[<InlineData("Day4/testInput.txt", -1)>]
[<InlineData("Day4/input.txt", -1)>]
let ``test`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day4/testInput.txt", -1)>]
[<InlineData("Day4/input.txt", -1)>]
let ``test 2`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)

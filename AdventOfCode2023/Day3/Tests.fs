module day3.Tests

open Xunit

[<Theory>]
[<InlineData("day3/testInput.txt", 0)>]
[<InlineData("day3/input.txt", 0)>]
let ``test`` (filename: string, expected: int) =
  let result = -1
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("day3/testInput.txt", 0)>]
[<InlineData("day3/input.txt", 0)>]
let ``test 2`` (filename: string, expected: int) =
  let result = -1
  Assert.Equal(expected, result)

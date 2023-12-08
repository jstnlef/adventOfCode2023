module Day8.Tests

open Xunit

[<Theory>]
[<InlineData("Day8/testInput.txt", 2)>]
[<InlineData("Day8/testInput2.txt", 6)>]
[<InlineData("Day8/input.txt", -1)>]
let ``Number of steps required to reach ZZZ`` (filename: string, expected: int) =
  let result = filename |> WastelandMap.parse |> WastelandMap.findWayOut
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day8/testInput.txt", -1)>]
[<InlineData("Day8/input.txt", -1)>]
let ``test 2`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)

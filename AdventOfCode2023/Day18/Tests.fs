module Day18.Tests

open Xunit

[<Theory>]
[<InlineData("Day18/testInput.txt", 62)>]
[<InlineData("Day18/input.txt", 48400)>]
let ``The cubic meters of lava the dug out area could hold`` (filename: string, expected: int) =
  let result = filename |> DigPlan.parse |> DigPlan.dugOutArea
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day18/testInput.txt", -1)>]
[<InlineData("Day18/input.txt", -1)>]
let ``test 2`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)

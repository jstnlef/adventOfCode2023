module Day13.Tests

open Xunit

[<Theory>]
[<InlineData("Day13/testInput.txt", 405)>]
[<InlineData("Day13/input.txt", 34889)>]
let ``The sum of the lines of reflection for all the rock patterns`` (filename: string, expected: int) =
  let result = filename |> RockFormations.parse |> RockFormations.countReflections
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day13/testInput.txt", -1)>]
[<InlineData("Day13/input.txt", -1)>]
let ``test 2`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)

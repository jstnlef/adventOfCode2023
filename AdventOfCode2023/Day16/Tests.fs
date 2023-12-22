module Day16.Tests

open Xunit

[<Theory>]
[<InlineData("Day16/testInput.txt", 46)>]
[<InlineData("Day16/input.txt", -1)>]
let ``Count the number of energized tiles`` (filename: string, expected: int) =
  let result = filename |> Contraption.parse |> Contraption.countEnergizedTiles
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day16/testInput.txt", -1)>]
[<InlineData("Day16/input.txt", -1)>]
let ``test 2`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)

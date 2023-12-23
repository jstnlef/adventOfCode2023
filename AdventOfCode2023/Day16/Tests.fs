module Day16.Tests

open Xunit

[<Theory>]
[<InlineData("Day16/testInput.txt", 46)>]
[<InlineData("Day16/input.txt", 7482)>]
let ``Count the number of energized tiles`` (filename: string, expected: int) =
  let result =
    filename |> Contraption.parse |> Contraption.countEnergizedTiles (-1, 0, 1, 0)

  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day16/testInput.txt", 51)>]
[<InlineData("Day16/input.txt", 7896)>]
let ``The largest number of energized tiles`` (filename: string, expected: int) =
  let result = filename |> Contraption.parse |> Contraption.findMaxEnergizedTiles
  Assert.Equal(expected, result)

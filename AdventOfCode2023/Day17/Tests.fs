module Day17.Tests

open Xunit

[<Theory>]
[<InlineData("Day17/testInput.txt", 102)>]
[<InlineData("Day17/input.txt", 1244)>]
let ``The least amount of heat loss while traversing the city`` (filename: string, expected: int) =
  let result = filename |> HeatLossMap.parse |> HeatLossMap.findMinHeatLoss 1 3
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day17/testInput.txt", -1)>]
[<InlineData("Day17/input.txt", -1)>]
let ``test 2`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)

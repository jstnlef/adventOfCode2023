module Day17.Tests

open Xunit

[<Theory>]
[<InlineData("Day17/testInput.txt", 102)>]
[<InlineData("Day17/input.txt", 1244)>]
let ``The least amount of heat loss while traversing the city`` (filename: string, expected: int) =
  let result = filename |> HeatLossMap.parse |> HeatLossMap.findMinHeatLoss 1 3
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day17/testInput.txt", 94)>]
[<InlineData("Day17/testInput2.txt", 71)>]
[<InlineData("Day17/input.txt", 1367)>]
let ``The least amount of heat loss while traversing the city with an ultra crucible``
  (
    filename: string,
    expected: int
  ) =
  let result = filename |> HeatLossMap.parse |> HeatLossMap.findMinHeatLoss 4 10
  Assert.Equal(expected, result)

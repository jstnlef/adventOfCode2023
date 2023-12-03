module Day3.Tests

open Xunit

[<Theory>]
[<InlineData("Day3/testInput.txt", 4361)>]
[<InlineData("Day3/input.txt", 526404)>]
let ``Sum of all the actual part numbers`` (filename: string, expected: int) =
  let result =
    filename |> Schematic.parse |> Schematic.allActualPartNumbers |> Seq.sum

  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day3/testInput.txt", 467835)>]
[<InlineData("Day3/input.txt", 84399773)>]
let ``Sum of all the gear ratios`` (filename: string, expected: int) =
  let result = filename |> Schematic.parse |> Schematic.allGearRatios |> Seq.sum
  Assert.Equal(expected, result)

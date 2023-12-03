module Day3.Tests

open Xunit

[<Theory>]
[<InlineData("Day3/testInput.txt", 4361)>]
[<InlineData("Day3/input.txt", 526404)>]
let ``Sum of all the part numbers`` (filename: string, expected: int) =
  let result =
    filename |> Schematic.parse |> Schematic.allActualPartNumbers |> Seq.sum

  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day3/testInput.txt", 0)>]
[<InlineData("Day3/input.txt", 0)>]
let ``test 2`` (filename: string, expected: int) =
  let result = -1
  Assert.Equal(expected, result)

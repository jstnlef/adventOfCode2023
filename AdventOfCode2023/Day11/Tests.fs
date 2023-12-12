module Day11.Tests

open Xunit

[<Theory>]
[<InlineData("Day11/testInput.txt", 374)>]
[<InlineData("Day11/input.txt", -1)>]
let ``The sum of these lengths between each pair of galaxies`` (filename: string, expected: int) =
  let result = filename |> Image.parse |> Image.allShortestDistances |> Seq.sum
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day11/testInput.txt", -1)>]
[<InlineData("Day11/input.txt", -1)>]
let ``test 2`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)

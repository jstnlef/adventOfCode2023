module Day10.Tests

open Xunit

[<Theory>]
[<InlineData("Day10/testInput.txt", 4)>]
[<InlineData("Day10/testInput2.txt", 8)>]
[<InlineData("Day10/input.txt", 6842)>]
let ``Number of steps to get to the farthest point in the pipe loop`` (filename: string, expected: int) =
  let result = filename |> Pipes.parse |> Pipes.distanceToFarthestPoint
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day10/testInput.txt", -1)>]
[<InlineData("Day10/input.txt", -1)>]
let ``test 2`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)

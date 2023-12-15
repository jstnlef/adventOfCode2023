module Day11.Tests

open Xunit

[<Theory>]
[<InlineData("Day11/testInput.txt", 374)>]
[<InlineData("Day11/input.txt", 9769724)>]
let ``The sum of these lengths between each pair of galaxies`` (filename: string, expected: int64) =
  let result = filename |> Image.parse 2 |> Image.allShortestDistances |> Seq.sum
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day11/testInput.txt", 10, 1030)>]
[<InlineData("Day11/testInput.txt", 100, 8410)>]
[<InlineData("Day11/input.txt", 1_000_000, 603020563700L)>]
let ``The sum of these lengths between each pair of galaxies with an expansion factor``
  (
    filename: string,
    expansion: int,
    expected: int64
  ) =
  let result =
    filename |> Image.parse expansion |> Image.allShortestDistances |> Seq.sum

  Assert.Equal(expected, result)

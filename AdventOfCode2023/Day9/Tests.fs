module Day9.Tests

open Xunit

[<Theory>]
[<InlineData("Day9/testInput.txt", 114)>]
[<InlineData("Day9/input.txt", 1930746032)>]
let ``The sum of the future extrapolated values`` (filename: string, expected: int) =
  let result =
    filename
    |> OASISReport.parse
    |> (OASISReport.predictedValues OASISReport.next)
    |> Array.sum

  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day9/testInput.txt", 2)>]
[<InlineData("Day9/input.txt", 1154)>]
let ``The sum of the past extrapolated values`` (filename: string, expected: int) =
  let result =
    filename
    |> OASISReport.parse
    |> (OASISReport.predictedValues OASISReport.previous)
    |> Array.sum

  Assert.Equal(expected, result)

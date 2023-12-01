module Day1.Tests

open Xunit

[<Theory>]
[<InlineData("Day1/testInput.txt", 142)>]
[<InlineData("Day1/input.txt", 55712)>]
let ``The sum of calibration values`` (filename: string, expected: int) =
  let result = CalibrationDocument.parse filename |> Seq.sum
  Assert.Equal(expected, result)

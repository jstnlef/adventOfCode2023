module Day1.Tests

open Xunit

[<Theory>]
[<InlineData("Day1/testInput.txt", 142)>]
[<InlineData("Day1/input.txt", 55712)>]
let ``The sum of calibration values`` (filename: string, expected: int) =
  let result =
    filename
    |> CalibrationDocument.parse CalibrationDocument.transformOnlyDigits
    |> Seq.sum

  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day1/testInput2.txt", 281)>]
[<InlineData("Day1/input.txt", 55413)>]
let ``The sum of calibration values with word digits`` (filename: string, expected: int) =
  let result =
    filename
    |> CalibrationDocument.parse CalibrationDocument.transformWithWordDigits
    |> Seq.sum

  Assert.Equal(expected, result)

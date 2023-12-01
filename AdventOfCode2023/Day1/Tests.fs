module Day1.Tests

open System
open Xunit

[<Theory>]
[<InlineData("Day1/testInput.txt", 142)>]
[<InlineData("Day1/input.txt", 55712)>]
let ``The sum of calibration values`` (filename: string, expected: int) =
  let result =
    CalibrationDocument.parse (String.filter Char.IsDigit) filename |> Seq.sum

  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day1/testInput2.txt", 281)>]
[<InlineData("Day1/input.txt", 55413)>]
let ``The sum of calibration values with word digits`` (filename: string, expected: int) =
  let transformToDigits (s: string) =
    let findWordDigit (subString: string) =
      CalibrationDocument.wordDigits.Keys
      |> Seq.tryFind subString.StartsWith
      |> Option.map (fun w -> CalibrationDocument.wordDigits[w])

    let mutable ret = []

    for i, c in s |> Seq.indexed do
      if Char.IsDigit c then
        ret <- ret @ [ Char.ToString c ]
      else
        match findWordDigit s[i..] with
        | Some(d) -> ret <- ret @ [ d ]
        | _ -> ()

    String.Join("", ret)

  let result = CalibrationDocument.parse transformToDigits filename |> Seq.sum
  Assert.Equal(expected, result)

module Day12.Tests

open Xunit

[<Theory>]
[<InlineData("Day12/testInput.txt", 21)>]
[<InlineData("Day12/input.txt", 7922)>]
let ``The sum of the different arrangements of springs`` (filename: string, expected: int64) =
  let result =
    filename
    |> ConditionRecord.parse false
    |> ConditionRecord.numberOfArrangementsPerRow
    |> Seq.sum

  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day12/testInput.txt", 525152)>]
[<InlineData("Day12/input.txt", 18093821750095L)>]
let ``The sum of the different arrangements of springs when expanded`` (filename: string, expected: int64) =
  let result =
    filename
    |> ConditionRecord.parse true
    |> ConditionRecord.numberOfArrangementsPerRow
    |> Seq.sum

  Assert.Equal(expected, result)

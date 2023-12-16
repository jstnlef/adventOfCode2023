module Day12.Tests

open Xunit

[<Theory>]
[<InlineData("Day12/testInput.txt", 21)>]
[<InlineData("Day12/input.txt", -1)>]
let ``The sum of the different arrangements of springs`` (filename: string, expected: int) =
  let result =
    filename
    |> ConditionRecord.parse
    |> ConditionRecord.numberOfArrangementsPerRow
    |> Seq.sum

  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day12/testInput.txt", -1)>]
[<InlineData("Day12/input.txt", -1)>]
let ``test 2`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)

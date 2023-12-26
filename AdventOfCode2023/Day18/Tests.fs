module Day18.Tests

open Xunit

[<Theory>]
[<InlineData("Day18/testInput.txt", 62)>]
[<InlineData("Day18/input.txt", 48400)>]
let ``The cubic meters of lava the dug out area could hold`` (filename: string, expected: int64) =
  let result =
    filename |> DigPlan.parse DigPlan.parseLineWithBug |> DigPlan.dugOutArea

  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day18/testInput.txt", 952408144115L)>]
[<InlineData("Day18/input.txt", 72811019847283L)>]
let ``The cubic meters of lava the dug out area could hold with corrected instructions``
  (
    filename: string,
    expected: int64
  ) =
  let result =
    filename |> DigPlan.parse DigPlan.parseLineCorrected |> DigPlan.dugOutArea

  Assert.Equal(expected, result)

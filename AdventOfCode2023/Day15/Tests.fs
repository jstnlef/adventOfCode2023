module Day15.Tests

open Xunit

[<Theory>]
[<InlineData("Day15/testInput.txt", 1320)>]
[<InlineData("Day15/input.txt", 506869)>]
let ``The sum of the hashes of each of the initialization steps`` (filename: string, expected: int) =
  let result = filename |> Initialization.parse |> Initialization.hashAllSteps
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day15/testInput.txt", 145)>]
[<InlineData("Day15/input.txt", 271384)>]
let ``The focusing power of the resulting lens configuration`` (filename: string, expected: int) =
  let result =
    filename |> Initialization.parse |> Initialization.run |> Lenses.focusingPower

  Assert.Equal(expected, result)

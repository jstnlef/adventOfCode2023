module Day20.Tests

open Xunit

[<Theory>]
[<InlineData("Day20/testInput.txt", 32000000)>]
[<InlineData("Day20/testInput2.txt", 11687500)>]
[<InlineData("Day20/input.txt", -1)>]
let ``Multiply the total number of low pulses and high pulses`` (filename: string, expected: int64) =
  let result = filename |> ModuleConfig.parse |> ModuleConfig.countPulses
  Assert.Equal(expected, result)


[<Theory>]
[<InlineData("Day20/testInput.txt", -1)>]
[<InlineData("Day20/input.txt", -1)>]
let ``test 2`` (filename: string, expected: int) =
  let result = 0
  Assert.Equal(expected, result)

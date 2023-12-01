module Day1.Tests

open Xunit

[<Theory>]
[<InlineData("Day1/testInput.txt", 142)>]
[<InlineData("Day1/input.txt", 66487)>]
let ``My test`` (filename: string, expected: int) =
    Assert.True(false)

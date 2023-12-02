module Day2.Tests

open Xunit

[<Theory>]
[<InlineData("Day2/testInput.txt", 8)>]
[<InlineData("Day2/input.txt", 2283)>]
let ``Sum of the ids of the possible games`` (filename: string, expected: int) =
  let result =
    filename
    |> Games.parse
    |> Seq.filter Games.gameIsPossible
    |> Seq.sumBy (fun g -> g.id)

  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day2/testInput.txt", 2286)>]
[<InlineData("Day2/input.txt", 78669)>]
let ``Sum of powers of the fewest number of cubes of each color`` (filename: string, expected: int) =
  let result =
    filename |> Games.parse |> Seq.sumBy (Games.findFewestCubes >> Cubes.multiply)

  Assert.Equal(expected, result)

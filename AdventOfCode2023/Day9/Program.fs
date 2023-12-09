namespace Day9

open System.IO

module OASISReport =
  let parse filename = filename |> File.ReadLines

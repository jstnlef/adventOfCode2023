namespace Day1

open System
open System.IO

type CalibrationValue = int
type CalibrationDocument = int seq

module CalibrationDocument =
  let wordDigits =
    Map["one", "1"
        "two", "2"
        "three", "3"
        "four", "4"
        "five", "5"
        "six", "6"
        "seven", "7"
        "eight", "8"
        "nine", "9"]

  let parse predicate filename : CalibrationDocument =
    let findValue (transform: string -> string) line =
      let digits = line |> transform
      String [| digits[0]; digits[digits.Length - 1] |] |> Int32.Parse

    File.ReadLines filename |> Seq.map (findValue predicate)

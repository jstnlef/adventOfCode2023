namespace Day1

open System
open System.IO

type CalibrationValue = int
type CalibrationDocument = int seq

module CalibrationDocument =
  let parse filename : CalibrationDocument =
    let findValue line =
      let digits = line |> String.filter Char.IsDigit
      String [| digits[0]; digits[digits.Length - 1] |] |> Int32.Parse

    File.ReadLines filename |> Seq.map findValue

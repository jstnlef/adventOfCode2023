namespace Day1

open System
open System.Collections.Generic
open System.IO

type CalibrationValue = int
type CalibrationDocument = int seq

module CalibrationDocument =
  let wordDigits =
    Map["one", '1'
        "two", '2'
        "three", '3'
        "four", '4'
        "five", '5'
        "six", '6'
        "seven", '7'
        "eight", '8'
        "nine", '9']

  let transformOnlyDigits = (String.filter Char.IsDigit)

  let transformWithWordDigits (s: String) =
    let findWordDigit (subString: string) =
      wordDigits.Keys
      |> Seq.tryFind subString.StartsWith
      |> Option.map (fun w -> wordDigits[w])

    let ret = List<char>()

    for i, c in s |> Seq.indexed do
      if Char.IsDigit c then
        ret.Add c
      else
        match findWordDigit s[i..] with
        | Some(d) -> ret.Add d
        | None -> ()

    String(ret.ToArray())

  let parse predicate filename : CalibrationDocument =
    let findValue (transform: string -> string) line =
      let digits = line |> transform
      String [| digits[0]; digits[digits.Length - 1] |] |> Int32.Parse

    File.ReadLines filename |> Seq.map (findValue predicate)

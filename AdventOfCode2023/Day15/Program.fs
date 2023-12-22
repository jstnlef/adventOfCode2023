namespace Day15

open System
open System.IO
open System.Text.RegularExpressions

type Lens = string * int
type Box = Lens list
type Lenses = Box array

module Lenses =
  let init () = Array.init 256 (fun _ -> List.empty)

  let focusingPower (lenses: Lenses) =
    let focusingPowerOfBox (boxi, box) =
      box
      |> List.indexed
      |> List.sumBy (fun (lensi, (_, focalLength)) -> (boxi + 1) * (lensi + 1) * focalLength)

    lenses |> Array.indexed |> Array.sumBy focusingPowerOfBox

module Initialization =
  let hash = Seq.fold (fun current c -> (current + int c) * 17 % 256) 0

  let hashAllSteps: string array -> int = Array.sumBy hash

  let regex = Regex("^(?<label>\w+)(?<op>[=|-])(?<val>\d*)$")

  let runStep (lenses: Lenses) step =
    let m = regex.Match step
    let label = m.Groups["label"].Value
    let boxIndex = hash label

    match m.Groups["op"].Value with
    | "-" -> lenses[boxIndex] <- lenses[boxIndex] |> List.filter (fun (l, _) -> label <> l)
    | "=" ->
      let focalLength = Int32.Parse(m.Groups["val"].Value)

      match List.tryFind (fun (l, _) -> label = l) lenses[boxIndex] with
      | Some _ ->
        lenses[boxIndex] <-
          lenses[boxIndex]
          |> List.map (fun (l, f) -> if l = label then l, focalLength else l, f)
      | None -> lenses[boxIndex] <- lenses[boxIndex] @ [ (label, focalLength) ]
    | _ -> failwith "Invalid op"

    lenses

  let run steps =
    steps |> Array.fold runStep (Lenses.init ())

  let parse filename =
    filename |> File.ReadAllText |> _.Trim().Split(",")

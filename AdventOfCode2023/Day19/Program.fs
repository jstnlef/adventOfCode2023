namespace Day19

open System
open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Collections

type Op =
  | LessThan
  | GreaterThan

type Transition =
  | Accepted
  | Rejected
  | Rule of string

module Transition =
  let parse s =
    match s with
    | "A" -> Accepted
    | "R" -> Rejected
    | s -> Rule(s)

module Op =
  let parse s =
    match s with
    | "<" -> LessThan
    | ">" -> GreaterThan
    | _ -> failwith "Unknown op"

type Rule =
  { category: string
    op: Op
    value: int64
    transition: Transition }

type Part =
  { x: int64
    m: int64
    a: int64
    s: int64 }

module Part =
  let ratingSum part = part.x + part.m + part.a + part.s

  let regex = Regex("^{x=(?<x>\d+),m=(?<m>\d+),a=(?<a>\d+),s=(?<s>\d+)}$")

  let parse line =
    let m = regex.Match(line)

    { x = Int32.Parse(m.Groups["x"].Value)
      m = Int32.Parse(m.Groups["m"].Value)
      a = Int32.Parse(m.Groups["a"].Value)
      s = Int32.Parse(m.Groups["s"].Value) }

type PartRange =
  { x: int64 * int64
    m: int64 * int64
    a: int64 * int64
    s: int64 * int64 }

module Rule =
  let evaluateRuleForRange (range: PartRange) rule : (Transition * PartRange) * PartRange =
    let split (rMin, rMax) =
      match rule.op with
      | GreaterThan -> ((max (rule.value + 1L) rMin), rMax), (rMin, (min rule.value rMax))
      | LessThan -> (rMin, (min (rule.value - 1L) rMax)), ((max rule.value rMin), rMax)

    let matched, unmatched =
      match rule.category with
      | "x" ->
        let a, r = split range.x
        { range with x = a }, { range with x = r }
      | "m" ->
        let a, r = split range.m
        { range with m = a }, { range with m = r }
      | "a" ->
        let a, r = split range.a
        { range with a = a }, { range with a = r }
      | "s" ->
        let a, r = split range.s
        { range with s = a }, { range with s = r }
      | _ -> failwith "Unknown category"

    (rule.transition, matched), unmatched

  let evaluate (part: Part) rule =
    let rating =
      match rule.category with
      | "x" -> part.x
      | "m" -> part.m
      | "a" -> part.a
      | "s" -> part.s
      | _ -> failwith "Unknown category"

    match rule.op with
    | GreaterThan -> rating > rule.value
    | LessThan -> rating < rule.value

type Workflow =
  { name: string
    rules: Rule array
    defaultTransition: Transition }

module Workflow =
  let evaluate part (workflows: Map<string, Workflow>) =
    let rec evaluateWithWorkflow workflow =
      let transition =
        match Array.tryFind (Rule.evaluate part) workflow.rules with
        | Some(r) -> r.transition
        | None -> workflow.defaultTransition

      match transition with
      | Rule(name) -> evaluateWithWorkflow workflows[name]
      | t -> t

    evaluateWithWorkflow workflows["in"]

  let ratingMin = 1L
  let ratingMax = 4000L

  let findAcceptedRatingsRanges (workflows: Map<string, Workflow>) =
    let rec findRangesWithWorkflow ranges =
      match ranges with
      | [] -> []
      | (Rejected, _) :: rest -> findRangesWithWorkflow rest
      | (Accepted, range) :: rest -> (Accepted, range) :: findRangesWithWorkflow rest
      | (Rule(name), range) :: rest ->
        let workflow = workflows[name]
        let mutable nextToEval = range
        let mutable nextRanges = rest

        for rule in workflow.rules do
          let a, b = Rule.evaluateRuleForRange nextToEval rule
          nextRanges <- a :: nextRanges
          nextToEval <- b

        nextRanges <- (workflow.defaultTransition, nextToEval) :: nextRanges
        findRangesWithWorkflow nextRanges

    let initRange =
      { x = ratingMin, ratingMax
        m = ratingMin, ratingMax
        a = ratingMin, ratingMax
        s = ratingMin, ratingMax }

    findRangesWithWorkflow [ (Rule("in"), initRange) ] |> List.map snd

  let regex =
    Regex("^(?<name>\w+){(?:(?<cat>\w)(?<op>[<|>])(?<num>\d+):(?<transition>\w+),*)+(?<defaultTransition>\w+)}$")

  let parse line =
    let m = regex.Match(line)

    let rules =
      seq { 0 .. m.Groups["cat"].Captures.Count - 1 }
      |> Seq.map (fun i ->
        { category = m.Groups["cat"].Captures[i].Value
          op = m.Groups["op"].Captures[i].Value |> Op.parse
          value = m.Groups["num"].Captures[i].Value |> Int64.Parse
          transition = m.Groups["transition"].Captures[i].Value |> Transition.parse })
      |> Seq.toArray

    { name = m.Groups["name"].Value
      rules = rules
      defaultTransition = m.Groups["defaultTransition"].Value |> Transition.parse }

type System =
  { workflows: Map<string, Workflow>
    parts: Part array }

module System =
  let sumOfAcceptedParts system =
    system.parts
    |> Array.filter (fun part -> Workflow.evaluate part system.workflows = Accepted)
    |> Array.sumBy Part.ratingSum

  let combinationsOfAcceptedRatings system : int64 =
    system.workflows
    |> Workflow.findAcceptedRatingsRanges
    |> Seq.sumBy (fun ranges ->
      let xMin, xMax = ranges.x
      let mMin, mMax = ranges.m
      let aMin, aMax = ranges.a
      let sMin, sMax = ranges.s

      (xMax - xMin + 1L)
      * (mMax - mMin + 1L)
      * (aMax - aMin + 1L)
      * (sMax - sMin + 1L))

  let parse filename =
    let lines = filename |> File.ReadAllLines
    let splitIndex = Array.findIndex (fun line -> line = "") lines
    let workflowLines, partsLines = Array.splitAt splitIndex lines
    let workflows = workflowLines |> Array.map Workflow.parse
    let parts = partsLines[1..] |> Array.map Part.parse

    { workflows = workflows |> Array.map (fun w -> w.name, w) |> Map
      parts = parts }

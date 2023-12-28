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
    value: int
    transition: Transition }

type Part = { x: int; m: int; a: int; s: int }

module Part =
  let ratingSum part = part.x + part.m + part.a + part.s
  let regex = Regex("^{x=(?<x>\d+),m=(?<m>\d+),a=(?<a>\d+),s=(?<s>\d+)}$")

  let parse line =
    let m = regex.Match(line)

    { x = Int32.Parse(m.Groups["x"].Value)
      m = Int32.Parse(m.Groups["m"].Value)
      a = Int32.Parse(m.Groups["a"].Value)
      s = Int32.Parse(m.Groups["s"].Value) }

module Rule =
  let evaluate part rule =
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

  let regex =
    Regex("^(?<name>\w+){(?:(?<cat>\w)(?<op>[<|>])(?<num>\d+):(?<transition>\w+),*)+(?<defaultTransition>\w+)}$")

  let parse line =
    let m = regex.Match(line)

    let rules =
      seq { 0 .. m.Groups["cat"].Captures.Count - 1 }
      |> Seq.map (fun i ->
        { category = m.Groups["cat"].Captures[i].Value
          op = m.Groups["op"].Captures[i].Value |> Op.parse
          value = m.Groups["num"].Captures[i].Value |> Int32.Parse
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

  let parse filename =
    let lines = filename |> File.ReadAllLines
    let splitIndex = Array.findIndex (fun line -> line = "") lines
    let workflowLines, partsLines = Array.splitAt splitIndex lines
    let workflows = workflowLines |> Array.map Workflow.parse
    let parts = partsLines[1..] |> Array.map Part.parse

    { workflows = workflows |> Array.map (fun w -> w.name, w) |> Map
      parts = parts }

namespace Day20

open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

type Voltage =
  | High
  | Low

type Pulse =
  { src: string
    dest: string
    voltage: Voltage }

type Broadcaster = { name: string; outputs: string array }

type FlipFlop =
  { name: string
    isOn: bool
    outputs: string array }

  member this.Toggle() = { this with isOn = not this.isOn }

type Conjunction =
  { name: string
    memory: Map<string, Voltage>
    outputs: string array }

  member this.UpdateMemory(pulse: Pulse) =
    { this with
        memory =
          Map.change
            pulse.src
            (fun maybeCharge ->
              match maybeCharge with
              | Some _ -> Some(pulse.voltage)
              | None -> failwith "Unknown input!")
            this.memory }

type Module =
  | Broadcaster of Broadcaster
  | FlipFlop of FlipFlop
  | Conjunction of Conjunction

type Modules = Map<string, Module>

type State =
  { pulses: Queue<Pulse>
    modules: Modules
    lowPulseCount: int
    highPulseCount: int
    buttonPushes: int
    lowToRx: bool }

  member this.PushButton() =
    let updated =
      this.SendPulse(
        { src = "button"
          dest = "broadcaster"
          voltage = Low }
      )

    { updated with
        buttonPushes = this.buttonPushes + 1 }

  member this.SendPulse(pulse: Pulse) =
    this.pulses.Enqueue(pulse)

    match pulse.voltage with
    | Low ->
      { this with
          lowPulseCount = this.lowPulseCount + 1 }
    | High ->
      { this with
          highPulseCount = this.highPulseCount + 1 }

  member this.UpdateModule(name: string, m: Module) =
    { this with
        modules = this.modules.Add(name, m) }

module Modules =
  let init modules : State =
    { pulses = Queue()
      modules = modules
      lowPulseCount = 0
      highPulseCount = 0
      buttonPushes = 0
      lowToRx = false }

  let doBroadcast (broadcast: Broadcaster) state =
    broadcast.outputs
    |> Array.fold
      (fun (s: State) output ->
        s.SendPulse(
          { src = broadcast.name
            dest = output
            voltage = Low }
        ))
      state

  let doFlipFlop (flipFlop: FlipFlop) pulse (state: State) =
    if pulse.voltage = Low then
      let updated = flipFlop.Toggle()

      updated.outputs
      |> Array.fold
        (fun (s: State) output ->
          s.SendPulse(
            { src = updated.name
              dest = output
              voltage = if updated.isOn then High else Low }
          ))
        (state.UpdateModule(flipFlop.name, FlipFlop(updated)))
    else
      state

  let doConjunction (conjunction: Conjunction) pulse (state: State) =
    let updated = conjunction.UpdateMemory(pulse)

    let pulseVoltage =
      if updated.memory.Values |> Seq.forall (fun v -> v = High) then
        Low
      else
        High

    updated.outputs
    |> Array.fold
      (fun (s: State) output ->
        s.SendPulse(
          { src = updated.name
            dest = output
            voltage = pulseVoltage }
        ))
      (state.UpdateModule(updated.name, Conjunction(updated)))

  let processPulse pulse state =
    if pulse.dest = "rx" && pulse.voltage = Low then
      { state with lowToRx = true }
    else
      match Map.tryFind pulse.dest state.modules with
      | None -> state
      | Some(Broadcaster b) -> doBroadcast b state
      | Some(FlipFlop f) -> doFlipFlop f pulse state
      | Some(Conjunction c) -> doConjunction c pulse state

  let pushButton (initial: State) _ =
    let mutable state = initial.PushButton()
    let mutable pulse = { src = ""; dest = ""; voltage = Low }

    while state.pulses.TryDequeue(&pulse) do
      state <- processPulse pulse state

    state

  let countPulses modules =
    seq { 0..999 }
    |> Seq.fold pushButton (init modules)
    |> (fun s -> s.lowPulseCount * s.highPulseCount)

  let fewestButtonPushesToRx modules =
    Seq.initInfinite ((+) 0)
    |> Seq.scan pushButton (init modules)
    |> Seq.takeWhile (fun s -> not s.lowToRx)
    |> Seq.last
    |> _.buttonPushes

  let lineRegex =
    Regex("^(?<modType>broadcaster|[%|&])(?<name>\w+)* ->(?: (?<output>\w+),*)+$")

  let parse filename : Modules =
    let parseLine config line =
      let m = lineRegex.Match(line)
      let mutable name = m.Groups["name"].Value
      let outputs = m.Groups["output"].Captures |> Seq.map _.Value |> Seq.toArray

      let moduleType =
        match m.Groups["modType"].Value with
        | "broadcaster" ->
          name <- "broadcaster"
          Broadcaster { name = name; outputs = outputs }
        | "%" ->
          FlipFlop
            { name = name
              isOn = false
              outputs = outputs }
        | "&" ->
          Conjunction
            { name = name
              memory = Map.empty
              outputs = outputs }
        | _ -> failwith "Unknown module type"

      config |> Map.add name moduleType

    let setConjunctionInputs (modules: Modules) =
      let conjunctions =
        modules.Values
        |> Seq.map (fun m ->
          match m with
          | Module.Conjunction c -> c.name
          | _ -> "")
        |> Seq.filter (fun s -> s <> "")
        |> Set

      modules.Values
      |> Seq.fold
        (fun updated m ->
          let name, outputs =
            match m with
            | Broadcaster b -> b.name, Set b.outputs
            | FlipFlop f -> f.name, Set f.outputs
            | Conjunction c -> c.name, Set c.outputs

          Set.intersect conjunctions outputs
          |> Set.fold
            (fun (updated: Modules) n ->
              let c =
                match updated[n] with
                | Conjunction c ->
                  Conjunction
                    { c with
                        memory = Map.add name Low c.memory }
                | _ -> failwith "Shouldn't be here!"

              Map.add n c updated)
            updated)
        modules

    filename
    |> File.ReadLines
    |> Seq.fold parseLine Map.empty
    |> setConjunctionInputs

module Solutions.Days.Day20

open System.Collections.Generic
open AoCHelpers
open Microsoft.FSharp.Collections

type Name = string
type State = | On | Off
type Pulse = | High | Low
type CommunicationModule =
    | Broadcaster of Name list
    | FlipFlop of State * Name list
    | Conjunction of Map<Name, Pulse> * Name list
    
type Cycle = | NoCycle | StartCycle of float | FoundCycle of float
    
let parseModule (line: string) =
    let parts = line.Split("->")
    let m = parts[0].Trim()
    let destinations = split ',' parts[1] |> Seq.toList
    if m = "broadcaster" then
        m, Broadcaster(destinations |> Seq.toList)
    else
        let moduleType = m[0]
        let moduleName = m[1..]
        match moduleType with
        | '%' -> moduleName, FlipFlop(State.Off, destinations)
        | '&' -> moduleName, Conjunction(Map[], destinations)
        | c -> failwithf $"Invalid module type %c{c}"
            
let getOutputs = function
    | Broadcaster(outputs) -> outputs
    | FlipFlop(_, outputs) -> outputs
    | Conjunction(_, outputs) -> outputs
let postprocessConjunctionModules (modules: Map<Name, CommunicationModule>) =
    let conjunctionModules = modules |> Seq.choose (fun kvp -> match kvp.Value with | Conjunction (_,outputs) -> Some(kvp.Key, outputs) | _ -> None)
    let addInputs (modules: Map<Name, CommunicationModule>) (name, outputs) =
        let inputs = modules |> Seq.map (fun kvp -> kvp.Key, getOutputs kvp.Value) |> Seq.filter (fun (_,outputs) -> outputs |> List.contains name) |> Seq.map (fun (k,_) -> k, Pulse.Low) |> Map
        modules |> Map.add name (Conjunction(inputs, outputs)) 
        
    (modules, conjunctionModules) ||> Seq.fold addInputs
        
type CycleState = {
    TotalLowPulses: float
    TotalHighPulses: float
    Modules: Map<Name, CommunicationModule>
    ButtonPresses: float
    Cycles: Map<string, Cycle>
}

let modifyState state (name, newModule) =
    { state with Modules = state.Modules |> Map.add name newModule }
let pulsesToOutputs sender pulse outputs = outputs |> Seq.map (fun o -> (sender, o,pulse))

let addPulseCount state pulse =
    if pulse = Pulse.High then
        {state with TotalHighPulses = state.TotalHighPulses + 1. }
    else
        {state with TotalLowPulses =  state.TotalLowPulses + 1. }

let rec sendPulse queue state =
    match nextItem queue with
    | None -> state
    | Some(sender, name, pulse) ->
        let state = addPulseCount state pulse
            
        match state.Modules |> Map.tryFind name with
        | Some(Broadcaster outputs) -> handleBroadcaster name state queue pulse outputs
        | Some(FlipFlop(moduleState, outputs)) -> handleFlipFlop name state queue pulse moduleState outputs
        | Some(Conjunction(inputs, outputs)) -> handleConjunction name state queue pulse inputs outputs sender
        | None -> sendPulse queue state
                    
                    
and handleBroadcaster name state queue pulse outputs =
    outputs |> (pulsesToOutputs name pulse) |> (enqueue queue)
    sendPulse queue state
and handleFlipFlop name state queue pulse moduleState outputs =
    if pulse = Pulse.High then
        sendPulse queue state
    else if moduleState = State.On then
        outputs |> (pulsesToOutputs name Pulse.Low) |> (enqueue queue)
        (name,FlipFlop(State.Off, outputs)) |> (modifyState state) |> sendPulse queue
    else 
        outputs |> (pulsesToOutputs name Pulse.High) |> (enqueue queue)
        (name,FlipFlop(State.On, outputs)) |> (modifyState state) |> sendPulse queue
and handleConjunction name state queue pulse inputs outputs sender =
        
    let newModuleState = inputs |> Map.add sender pulse
    let newState = if newModuleState |> Seq.forall (fun kvp -> kvp.Value = Pulse.High) then
                        outputs |> (pulsesToOutputs name Pulse.Low) |> (enqueue queue)
                        state
                    else
                    
                        outputs |> (pulsesToOutputs name Pulse.High) |> (enqueue queue)
                        match state.Cycles |> Map.tryFind name with
                        | Some(NoCycle) when state.ButtonPresses <> 0 -> { state with Cycles = state.Cycles |> Map.add name (StartCycle(state.ButtonPresses)) }
                        | Some(StartCycle(presses)) -> { state with Cycles = state.Cycles |> Map.add name (FoundCycle(state.ButtonPresses - presses)) }
                        | _ -> state
        
    (name, Conjunction(newModuleState, outputs)) |> (modifyState newState) |> sendPulse queue
    
let pressButton state =
    let queue = Queue[("button", "broadcaster", Pulse.Low)]
    
    sendPulse queue state
   
let rec powerOnRx state =
    let completedCycles = state.Cycles |> Seq.choose (fun kvp -> match kvp.Value with | FoundCycle v -> Some(v) | _ -> None) |> Seq.toList
    if completedCycles.Length = state.Cycles.Count then
        lcmOfMultiple completedCycles
    else
        let state = pressButton state
        powerOnRx { state with ButtonPresses = state.ButtonPresses + 1. }
        
let run input =
    let modules = input |> Seq.map parseModule |> Map |> postprocessConjunctionModules
    let state = { TotalHighPulses = 0 ; TotalLowPulses = 0 ; Modules = modules ; ButtonPresses = 0 ; Cycles = Map[] }
    
    // Part 1
    
    let finalState = (state,[1..1000]) ||> Seq.fold (fun s _ -> pressButton s)
    let part1 = finalState.TotalHighPulses * finalState.TotalLowPulses
        
    // Part 2
    // Todo build cycle list from the module map
    let part2 = powerOnRx { state with Cycles = Map[("db", NoCycle);("sg", NoCycle); ("lm", NoCycle); ("dh", NoCycle)] }
    
    printResult part1 part2
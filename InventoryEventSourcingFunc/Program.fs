// Learn more about F# at http://fsharp.org

open System
open Model

[<EntryPoint>]
let main argv =
     let state = {InventoryBalance = InventoryBalanceUsingFIFO []}:State//  { Quantity=0;Value=0.0m; CostingMethod = CostingMethod.WeightedAverageCost 0.0m}
     let events= [ItensPurchased(2,10.0m); ItensPurchased(3,5.0m);ItensSold 4 ]
     let currentState = List.fold apply state events
     let command= Purchase(9, 10.0m)
     let newEvents = execute currentState command
     let finalState =  apply currentState newEvents;
     0 // return an integer exit code

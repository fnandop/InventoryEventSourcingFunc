//https://www.netsuite.com/portal/resource/articles/inventory-management/inventory-cost-accounting-methods-examples.shtml


module Model
open Microsoft.FSharp.Collections

    type Item  =  {Units:int; UnitCost:decimal; TotalCost:decimal }
    
    
    type InventoryBalance = 
        |InventoryBalanceUsingFIFO of Item list
        |InventoryBalanceUsingLIFO of Item list
        |InventoryBalanceUsinghWAC  of Item

    type State = { InventoryBalance:InventoryBalance}            
  
    type Event = 
        | ItensPurchased of  int*decimal
        | ItensSold of int

    

    type Command =
        |Purchase of int*decimal
        |Sell of int
    
    let onlyIfQuantityAvailable state qty  = 
            let availableQuantity = 
                match state.InventoryBalance with
                        | InventoryBalanceUsingFIFO items -> items |> List.sumBy (fun i->i.Units) 
                        | InventoryBalanceUsingLIFO items -> items |> List.sumBy (fun i->i.Units) 
                        | InventoryBalanceUsinghWAC item -> item.Units
            if  availableQuantity >= qty then 
                state 
            else  
                failwith "Out of stock"
    
    let execute state command = 
        let events = 
            match command with
                |Purchase(qty,unitCost) -> ItensPurchased(qty,unitCost)
                |Sell qty -> qty |> 
                             onlyIfQuantityAvailable state |>
                             (fun _ -> ItensSold qty)
        events;
    
    let rec consumeItems qty items =
        match (qty, items) with
            | (0, items) -> items
            | (qty, []) -> failwith "Out of stock"
            | (qty, h::t) -> if qty < h.Units then  {h with Units = h.Units-qty; TotalCost = (decimal (h.Units-qty)) * h.UnitCost  } :: t
                             else consumeItems (qty-h.Units) t
   

    let apply state event = 
        let inventoryBalance =   
            match event with
                | ItensPurchased(qty, unitCost)  ->  
                    match state.InventoryBalance with
                        | InventoryBalanceUsingFIFO items ->InventoryBalanceUsingFIFO (List.append items [{Units=qty; UnitCost = unitCost; TotalCost = (decimal qty) * unitCost }])
                        | InventoryBalanceUsingLIFO items -> InventoryBalanceUsingLIFO ({Units=qty; UnitCost = unitCost; TotalCost = (decimal qty) * unitCost }::items)
                        | InventoryBalanceUsinghWAC item -> 
                                    let totalUnits = qty + item.Units
                                    let totalCost  = (decimal qty)*unitCost + (decimal item.Units)*item.UnitCost
                                    let unitCost   = totalCost /(decimal totalUnits)
                                    InventoryBalanceUsinghWAC {Units=totalUnits ; UnitCost = unitCost; TotalCost = totalCost}



                | ItensSold qty  -> 
                    match state.InventoryBalance with
                        | InventoryBalanceUsingFIFO items ->InventoryBalanceUsingFIFO (consumeItems qty items)
                        | InventoryBalanceUsingLIFO items ->InventoryBalanceUsingLIFO (consumeItems qty items)
                        | InventoryBalanceUsinghWAC item -> InventoryBalanceUsinghWAC {item with Units=item.Units-qty ; TotalCost = item.TotalCost - (decimal qty)*item.UnitCost}
        
        {InventoryBalance=inventoryBalance}

    
         



module Tests

open Xunit
open Model

[<Fact>]
let ``Apply FIFO ItensPurchased Should Have the Right Balance`` () =
        //arrange 
        let initialState = {InventoryBalance = InventoryBalanceUsingFIFO []}
        let initialEvents = [ItensPurchased(50,100.00m);ItensPurchased(50,110.00m)]
        //act 
        let currentState = List.fold apply initialState initialEvents
        //assert
        let expectedState = {InventoryBalance = InventoryBalanceUsingFIFO [{Units = 50; UnitCost= 100.00m; TotalCost=5000.00m };{Units = 50; UnitCost= 110.00m; TotalCost=5500.00m }]}
        Assert.Equal(expectedState,currentState)


[<Fact>]
let ``Apply FIFO ItensSold Should Have the Right Balance`` () =
        //arrange 
        let initialState = {InventoryBalance = InventoryBalanceUsingFIFO []}
        let initialEvents = [ItensPurchased(50,100.00m);ItensPurchased(50,110.00m);ItensSold(60)]
        //act 
        let currentState = List.fold apply initialState initialEvents
        //assert
        let expectedState = {InventoryBalance = InventoryBalanceUsingFIFO [{Units = 40; UnitCost= 110.00m; TotalCost=4400.00m }]}
        Assert.Equal(expectedState,currentState)

[<Fact>]
let ``Apply LIFO ItensPurchased Should Have the Right Balance`` () =
        //arrange 
        let initialState = {InventoryBalance = InventoryBalanceUsingLIFO []}
        let initialEvents = [ItensPurchased(50,100.00m);ItensPurchased(50,110.00m)]
        //act 
        let currentState = List.fold apply initialState initialEvents
        //assert
        let expectedState = {InventoryBalance = InventoryBalanceUsingLIFO [{Units = 50; UnitCost= 110.00m; TotalCost=5500.00m };{Units = 50; UnitCost= 100.00m; TotalCost=5000.00m }]}
        Assert.Equal(expectedState,currentState)


[<Fact>]
let ``Apply LIFO ItensSold Should Have the Right Balance`` () =
        //arrange 
        let initialState = {InventoryBalance = InventoryBalanceUsingLIFO []}
        let initialEvents = [ItensPurchased(50,100.00m);ItensPurchased(50,110.00m);ItensSold(60)]
        //act 
        let currentState = List.fold apply initialState initialEvents
        //assert
        let expectedState = {InventoryBalance = InventoryBalanceUsingLIFO [{Units = 40; UnitCost= 100.00m; TotalCost=4000.00m }]}
        Assert.Equal(expectedState,currentState)


[<Fact>]
let ``Apply WAC ItensPurchased Should Have the Right Balance`` () =
        //arrange 
        let initialState = {InventoryBalance = InventoryBalanceUsinghWAC {Units =0; UnitCost=0.0m; TotalCost = 0.0m }}
        let initialEvents = [ItensPurchased(50,100.00m);ItensPurchased(50,110.00m)]
        //act 
        let currentState = List.fold apply initialState initialEvents
        //assert
        let expectedState = {InventoryBalance = InventoryBalanceUsinghWAC {Units = 100; UnitCost= 105.00m; TotalCost=10500.00m }}
        Assert.Equal(expectedState,currentState)


[<Fact>]
let ``Apply WAC ItensSold Should Have the Right Balance`` () =
        //arrange 
        let initialState = {InventoryBalance = InventoryBalanceUsinghWAC {Units =0; UnitCost=0.0m; TotalCost = 0.0m }}
        let initialEvents = [ItensPurchased(50,100.00m);ItensPurchased(50,110.00m);ItensSold(60)]
        //act 
        let currentState = List.fold apply initialState initialEvents
        //assert
        let expectedState = {InventoryBalance = InventoryBalanceUsinghWAC {Units = 40; UnitCost= 105.00m; TotalCost=4200.00m }}
        Assert.Equal(expectedState,currentState)
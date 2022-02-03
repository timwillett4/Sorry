open Sorry.Core.Tests.CreateGameTests
open Expecto
open System

[<EntryPoint>]
let main argv =
    let filter = 
        match argv with
        | [|_;filter|] -> fun (testName:string) -> testName.Contains filter
        | _ -> fun _ -> true

    createGameTests
    |> Test.filter filter
    |> runTests defaultConfig

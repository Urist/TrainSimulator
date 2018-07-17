// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Source

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    let RoundNumber = 0
    let NumberOfTrains = 2
    let TileSet = CreateTileSet RoundNumber

    let PlayerList = [1..NumberOfTrains] |> List.map (fun n -> StartTrain RoundNumber (sprintf "Train-%A" n) (TileSet |> Seq.take RackSize |> Seq.toList) )

    PlayTurn (List.head PlayerList) PlayerList


    0 // return an integer exit code

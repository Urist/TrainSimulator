module Source

type PlayerType = {
    Name : string
    Rack : List<int*int>
    TileList : List<int*int>
}

type PlayType = 
    (int*int)*PlayerType

type StrategyFunctionType =
    list<PlayType> -> list<PlayerType> -> PlayType*list<PlayerType>

let RackSize = 12

let HighestTileValue = 12

let CreateTileSet starter =
    seq [ for a in 0 .. HighestTileValue do
            for b in 0 .. a do
                // Don't generate the starter tile
                if not (a = b && b = starter) then
                    yield (a,b) ]


let IsSameTile (a,b) (x,y) =
    (a=x && b=y) || (b=x && a=y)


let FlipTile (a,b) =
    (b,a)


let IsValidPlay (a:int,b:int) (trainTiles:list<int*int>) =
    match trainTiles with
    | (x,y)::tail when x=a -> true
    | (x,y)::tail when x=b -> true
    | _ -> false


let ValidPlaysOnSingleTrain (hand:list<int*int>) train =
    hand 
        |> List.filter (fun tile -> IsValidPlay tile train.TileList)
        |> List.map (fun tile -> (tile, train))


let ValidPlaysOnAllTrains (hand:list<int*int>) trainList =
    trainList |> List.collect (fun t -> ValidPlaysOnSingleTrain hand t)


let StartTrain starter name rack =
        {Name = name; Rack = rack; TileList = [(starter,starter)]}:PlayerType


let AddTileToTrain (a,b) (train:list<int*int>) =
    match train with
    | (x,y)::tail when x=a -> (b,a)::train
    | (x,y)::tail when x=b -> (a,b)::train
    | _ -> failwithf "Tried to add non matching tile %A to train %A" (a,b) train


let DoPlayOnTrain activePlayer (tile:int*int,train) =
    let UpdatedPlayer:PlayerType = {Name = activePlayer.Name; Rack = activePlayer.Rack |> List.filter (fun t -> not (IsSameTile t tile)); TileList = activePlayer.TileList}
    let NewTrain:PlayerType = {Name = train.Name; Rack = train.Rack; TileList = AddTileToTrain tile train.TileList}
    (UpdatedPlayer, NewTrain)


let TrainsExcept trainList exceptTrain =
    trainList |> List.except (seq[yield exceptTrain])
    
let TrainsExceptPlay trainList (_,exceptTrain) =
    TrainsExcept trainList exceptTrain


let Pick =
    failwith "NOT IMPLEMENTED"
    0


let Strategy_AnyValid playList trainList =
    match playList with
    | (play,train)::plays -> 
        let chosenplay = (play,train)
        let othertrains = TrainsExcept trainList chosenplay
        (chosenplay, othertrains)
    | [] -> 
        failwith "ChoosePlay has no plays to choose from"


let Strategy_PlayOnShortest playList trainList =
    let play = playList |> List.minBy (fun (_,t) -> t.TileList |> List.length)
    let trains = TrainsExceptPlay trainList play
    (play, trains)


// It would be neat if strategies could be combined using function composition
let ChoosePlay (playList:list<PlayType>) trainList (strategyFunction:StrategyFunctionType) =
    strategyFunction playList trainList


let rec PlayTurn (activePlayer:PlayerType) openTrainList =
    printfn "Rack: %A" activePlayer.Rack
    printfn "== Open Trains =="
    openTrainList |> List.map (fun t -> printfn "%A: %A" t.Name t.TileList) |> ignore
    //printfn "== Closed Trains =="
    //closedTrainList |> List.map (fun t -> printfn "Train-%A: %A" t.Owner t.TileList) |> ignore

    let ValidPlays = ValidPlaysOnAllTrains activePlayer.Rack openTrainList
    match ValidPlays with
    | [] -> Pick
    | plays ->
        let (play, UnchangedTrains) = ChoosePlay plays openTrainList Strategy_PlayOnShortest
        printfn "Playing %A" play
        let (newPlayer, newTrain) = DoPlayOnTrain activePlayer play
        PlayTurn newPlayer (newTrain::UnchangedTrains)

//let PlayGame playerList =
    
module Source2

open TileSource

type Train =
    list<Tile>

type Rack =
    list<Tile>

type Action =
    | Draw
    | Play
    | PlayDouble

let IsValidPlay ((a,b):Tile) (train:Train) =
    match train with
    | (x,y)::tail when x=a -> true
    | (x,y)::tail when x=b -> true
    | _ -> false

let AddTileToTrain ((a,b):Tile) (train:Train) :Train =
    match train with
    | (x,y)::tail when x=a -> (b,a)::train
    | (x,y)::tail when x=b -> (a,b)::train
    | _ -> failwithf "Tried to add non matching tile %A to train %A" (a,b) train

let AddTileToRack (tile:Tile) rack:Rack =
    tile::rack

let RemoveTileFromRack (tile:Tile) rack:Rack =
    rack |> List.except (Seq.singleton tile)

let StartingTrain = (0,0)
let tileSource = new TileSource(0, 12)

let MapTiles =
    tileSource.Source |> Seq.mapFold (fun (rack,train) tile -> 
            match IsValidPlay tile train with
            | true  -> (tile, (rack, AddTileToTrain tile train))
            | false -> (tile, (AddTileToRack tile rack, train))
        ) 
        ([],[(0,0)])

let CreateCumulativeList =
    tileSource.Source |> Seq.mapFold (fun accum element -> (element::accum,element::accum)) []

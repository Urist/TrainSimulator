module TileSource

type Tile =
    (int*int)

type TileSource (StartValue:int, HighestTileValue:int) =

    let RandomSeed = 4

    let CreateTileSet starter =
        seq [ for a in 0 .. HighestTileValue do
                for b in 0 .. a do
                    // Don't generate the starter tile
                    if not (a = b && b = starter) then
                        yield (a,b) ]

    let RandomNumberSource =
        let Random = System.Random(RandomSeed)
        Seq.initInfinite (fun _ -> Random.Next())

    let RandomizedSeq inputSeq =
        Seq.zip RandomNumberSource inputSeq
        |> Seq.sortBy (fun (n,_) -> n)
        |> Seq.map (fun (_,n) -> n)

    member this.Source =
        RandomizedSeq (CreateTileSet StartValue)

    member this.AddTiles source tileList =
        List.toSeq tileList
        |> Seq.append source
        |> RandomizedSeq

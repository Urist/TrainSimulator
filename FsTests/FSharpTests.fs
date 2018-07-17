namespace FSharpTests

open Xunit
open FsCheck.Xunit

open Source

module Specification =

    [<Property>]
    let ``CreateTileSet generates correct number of tiles`` (x:int) =
        (Source.CreateTileSet x |> Seq.length) = ((HighestTileValue * HighestTileValue / 2) - 1)

    [<Property>]
    let ``CreateTileSet does not generate duplicates`` (x:int) =
        (Source.CreateTileSet x |> Seq.distinct |> Seq.length) = (Source.CreateTileSet x |> Seq.length)

    [<Property>]
    let ``FlipTile is reversible`` (x:int,y:int) =
        Source.FlipTile(Source.FlipTile (x,y)) = (x,y)

    [<Property>]
    let ``FlipTile flips the tile`` (x:int,y:int) =
        Source.FlipTile (x,y) = (y,x)

        
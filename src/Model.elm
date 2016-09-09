module Model exposing (..)

import Random
import Set exposing (Set, empty)

import Constants as Const


type alias Coordinate =
    { x : Int
    , y : Int
    }


type alias Model =
    { player : Int
    , robots : Set Int
    , junk : Set Int
    , alive : Bool
    , seed : Random.Seed
    , level : Int
    , score : Int
    }


init : Model
init =
    { player = 1379
    , robots = empty
    , junk = empty
    , alive = True
    , seed = Random.initialSeed 0
    , level = 1
    , score = 0
    }


{-- Helper Functions --}

intToCoord : Int -> Coordinate
intToCoord i =
    let i' = toFloat i
        rowSize = toFloat Const.gameWidth
        row = truncate (i' / rowSize)
        col = i % Const.gameWidth
    in
        { x = col, y = row }


coordToInt : Coordinate -> Int
coordToInt {x, y} =
    y * Const.gameWidth + x


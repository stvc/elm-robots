module Model exposing (..)

import Random


type alias Coordinate =
    { x : Int
    , y : Int
    }


type alias Model =
    { player : Coordinate
    , robots : List Coordinate
    , junk : List Coordinate
    , alive : Bool
    , seed : Random.Seed
    }


init : Model
init =
    { player = { x = 30, y = 20 }
    , robots = []
    , junk = []
    , alive = True
    , seed = Random.initialSeed 0
    }


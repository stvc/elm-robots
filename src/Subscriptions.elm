module Subscriptions exposing (subscriptions)

import Char
import Dict
import Keyboard
import Maybe

import Update exposing (Direction(..), Msg(Move, NoOp, Skip, Teleport))


subscriptions model =
    case model.alive of
        True ->
            Keyboard.downs keyToMsg
        False ->
            Sub.none


keyToMsg : Keyboard.KeyCode -> Msg
keyToMsg = -- Debug.log "keypress" >>
    Char.fromCode
        >> flip Dict.get keyCodeDict
        >> Maybe.withDefault NoOp


keyCodeDict = Dict.fromList
    [ ('K', Move N)
    , ('U', Move NE)
    , ('L', Move E)
    , ('N', Move SE)
    , ('J', Move S)
    , ('B', Move SW)
    , ('H', Move W)
    , ('Y', Move NW)
    , ('T', Teleport)
    , (Char.fromCode 190, Skip) -- for '.'
    ]


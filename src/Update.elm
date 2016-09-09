module Update exposing (..)

import Basics.Extra exposing (never)
import Maybe
import Random
import Task
import Time

import Constants as Const
import Model exposing (Model)


init =
    let fn = round >> Random.initialSeed >> SetSeed
    in
        Task.perform never fn Time.now


{-- Update --}

type Direction
    = N
    | NE
    | E
    | SE
    | S
    | SW
    | W
    | NW


type Msg
    = Move Direction
    | Skip
    | Teleport
    | SetSeed Random.Seed
    | NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            model ! []

        SetSeed newSeed ->
            { model
                | seed = newSeed
            } ! []

        Move direction ->
            let fn = movePlayer direction
                newModel = Maybe.withDefault model <| fn model
                _ = Debug.log "player coords" newModel.player
            in
                newModel ! []

        _ ->
            Debug.crash "not implemented"


movePlayer : Direction -> Model -> Maybe Model
movePlayer dir model =
    let co = model.player
        cn = case dir of
            N ->
                { x = co.x, y = co.y + 1 }

            NE ->
                { x = co.x + 1, y = co.y + 1 }

            E ->
                { x = co.x + 1, y = co.y }

            SE ->
                { x = co.x + 1, y = co.y - 1 }

            S ->
                { x = co.x, y = co.y - 1 }

            SW ->
                { x = co.x - 1, y = co.y - 1 }

            W ->
                { x = co.x - 1, y = co.y }

            NW ->
                { x = co.x - 1, y = co.y + 1 }

        xOutOfBounds = cn.x < 0 || cn.x >= Const.gameWidth
        yOutOfBounds = cn.y < 0 || cn.y >= Const.gameHeight

        isValid = not <| xOutOfBounds || yOutOfBounds
    in
        if isValid
            then
                Just { model | player = cn }
            else
                Nothing


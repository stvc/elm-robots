module Update exposing (Direction(..), Msg(..), init, update)

import Basics.Extra exposing (never)
import Maybe
import Random
import Set
import Task
import Time

import Constants as Const
import Model exposing (Model, intToCoord, coordToInt)


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
    | NewGame
    | NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            model ! []

        SetSeed newSeed ->
            update NewGame { model | seed = newSeed }

        NewGame ->
            generateNewGame model ! []

        Skip ->
            let filterDead m = if m.alive then Just m else Nothing
                fn m = filterDead m
                    `Maybe.andThen` moveRobotsMaybe
                    `Maybe.andThen` (updateLevel >> Just)

                newModel = fn model
                    |> Maybe.withDefault model
            in
                newModel ! []

        Move direction ->
            let filterDead m = if m.alive then Just m else Nothing

                fn m = filterDead m
                    `Maybe.andThen` movePlayer direction
                    `Maybe.andThen` moveRobotsMaybe
                    `Maybe.andThen` (updateLevel >> Just)

                newModel = fn model
                    |> Maybe.withDefault model
            in
                newModel ! []

        Teleport ->
            let newModel' = model
                    |> movePlayerToRandomLoc
                    |> moveRobots
                newModel = if model.alive then newModel' else model
            in
                { newModel
                    | alive = not <| checkIfPlayerDies newModel
                } ! []


coordGenerator : Random.Generator Int
coordGenerator =
    let maxCoord = Const.gameWidth * Const.gameHeight - 1
    in
        Random.int 0 maxCoord


generateNewGame : Model -> Model
generateNewGame =
    resetStats
        >> addRobots
        >> movePlayerToRandomLoc


generateNextLevel : Model -> Model
generateNextLevel model =
    { model
        | level = model.level + 1
        , junk = Set.empty
        , robots = Set.empty
    }
    |> addRobots
    |> movePlayerToRandomLoc


resetStats : Model -> Model
resetStats model =
    { model
        | level = 1
        , alive = True
        , score = 0
        , junk = Set.empty
        , robots = Set.empty
    }


updateLevel : Model -> Model
updateLevel model =
    if Set.size model.robots > 0
        then model
        else generateNextLevel model


addRobots : Model -> Model
addRobots model =
    let numRobots = 10 * model.level
        l = [1 .. numRobots]
    in
        List.foldr (always addRobot) model l


addRobot : Model -> Model
addRobot model =
    let (newRobot, newSeed) = Random.step coordGenerator model.seed
    in
        if Set.member newRobot model.robots
            then
                addRobot { model | seed = newSeed }
            else
                { model
                    | robots = Set.insert newRobot model.robots
                    , seed = newSeed
                }


movePlayerToRandomLoc : Model -> Model
movePlayerToRandomLoc model =
    let (newLoc, newSeed) = Random.step coordGenerator model.seed
        occupied = Set.member newLoc model.junk
            || Set.member newLoc model.robots
    in
        if occupied
            then
                movePlayerToRandomLoc { model | seed = newSeed }
            else
                { model
                    | player = newLoc
                    , seed = newSeed
                }


movePlayer : Direction -> Model -> Maybe Model
movePlayer dir model =
    let co = intToCoord model.player
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

        newPos = coordToInt cn
        unoccupied = not
            <| Set.member newPos model.junk
                || Set.member newPos model.robots
    in
        if isValid && unoccupied
            then
                Just { model | player = newPos }
            else
                Nothing


checkIfPlayerDies : Model -> Bool
checkIfPlayerDies model =
    Set.member model.player model.junk
        || Set.member model.player model.robots


moveRobotsMaybe : Model -> Maybe Model
moveRobotsMaybe model =
    let newModel = moveRobots model
    in
        if checkIfPlayerDies newModel
            then Nothing
            else Just newModel


moveRobots : Model -> Model
moveRobots model =
    let newLoc = moveTowards model.player
        fn robot (rs, js) =
            case (Set.member (newLoc robot) rs, Set.member (newLoc robot) js) of
                (_, True) ->
                    (rs, js)

                (True, _) ->
                    (Set.remove (newLoc robot) rs, Set.insert (newLoc robot) js)

                (False, False) ->
                    (Set.insert (newLoc robot) rs, js)

        (newRobots, newJunk) = Set.foldl fn (Set.empty, model.junk) model.robots

        pointsEarned = (*) 10
            <| (Set.size model.robots) - (Set.size newRobots)
    in
        { model
            | robots = newRobots
            , junk = newJunk
            , score = model.score + pointsEarned
        }

moveTowards : Int -> Int -> Int
moveTowards destination current =
    let dest = intToCoord destination
        curr = intToCoord current
        compare' s t = case compare s t of
            EQ ->
                s
            LT ->
                s + 1
            GT ->
                s - 1
        newX = compare' curr.x dest.x
        newY = compare' curr.y dest.y
    in
        coordToInt { x = newX, y = newY }


module View exposing (view)

import Collage exposing (Form, collage, move, moveX, moveY, group, text)
import Element exposing (toHtml)
import Html exposing (a, br, button, div, h1, p)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick)
import Set
import String
import Text as T

import Constants as Const
import Model
import Update exposing (Msg(NewGame))


view model =
    let collageWidth = Const.windowWidth * Const.charWidth
        collageHeight = Const.windowHeight * Const.charHeight

        corner = T.fromString "+"
        horizWall = T.fromString <| String.repeat Const.gameWidth "-"
        horizBorder = T.concat [ corner, horizWall, corner ]

        row = T.fromString <| String.repeat Const.gameWidth " "
        vertWall = T.fromString "|"
        rowWithWall = T.concat [ vertWall, row, vertWall ]
        rows = [ horizBorder ]
            ++ List.repeat Const.gameHeight rowWithWall
            ++ [ horizBorder ]

        chHeight = toFloat Const.charHeight
        gmHeight = toFloat Const.gameHeight
        chWidth = toFloat Const.charWidth

        styledRows =
            List.map textStyle rows

        translateFn r (i, rs) = (i + 1, (moveY (chHeight * i) r) :: rs)

        emptyGameBoard = List.foldr translateFn (0, []) styledRows
            |> snd
            |> group

        player = renderObject "@" model.player

        robots = model.robots
            |> Set.toList
            |> List.map (renderObject "+")
            |> group

        junk = model.junk
            |> Set.toList
            |> List.map (renderObject "*")
            |> group

        gameBoard = group [ emptyGameBoard, robots, junk, player ]
            |> moveY (-1 * (gmHeight + 1) * chHeight / 2.0)
            |> moveX (-8 * chWidth)


        rawHudLines = Const.instructionHudLines ++
            [ "Score: " ++ toString model.score
            , "Level: " ++ toString model.level
            , if model.alive then "Alive!" else "Dead!"
            ]

        renderHud = String.padRight Const.instructionHudWidth ' '
            >> T.fromString
            >> textStyle

        hudLines = List.map renderHud rawHudLines

        hud = List.foldr translateFn (0, []) hudLines
            |> snd
            |> group
            |> move Const.hudOffset

        content = group [ gameBoard, hud ]

        gameHtml = collage collageWidth collageHeight [ content ]
            |> toHtml
    in
        div [ style
                [ ("margin-left", "10px")
                , ("font-family", "monospace")
                ]
            ]
            [ h1 [ style [("font-family", "monospace")] ]
                [ Html.text "Robots" ]
            , gameHtml
            , br [] []
            , button
                [ style [("margin-left", "10px")]
                , onClick NewGame ]
                [ Html.text "New Game" ]
            , br [] []
            , p []
                [ a [ href "https://github.com/stvc/elm-robots" ] [ Html.text "Source code" ]
                ]
            ]


textStyle : T.Text -> Form
textStyle = T.monospace
    >> T.height Const.fontSize
    >> text


renderObject : String -> Int -> Form
renderObject char coords =
    let coords' = Model.intToCoord coords
            |> translateCoord
    in
        T.fromString char
            |> textStyle
            |> move coords'


translateCoord : Model.Coordinate -> (Float, Float)
translateCoord {x, y} =
    let chWidth = toFloat Const.charWidth
        gmWidth = toFloat Const.gameWidth

        x' = chWidth * ((toFloat x) - gmWidth / 2.0) + Const.xCorrection
        y' = (y + 1) * Const.charHeight
    in
        (x', y')


module View exposing (view)

import Collage exposing (Form, collage, move, moveX, moveY, group, text)
import Element exposing (toHtml)
import Set
import String
import Text as T

import Constants as Const
import Model


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
        translatedRows = List.foldr translateFn (0, []) styledRows
            |> snd

        emptyGameBoard = group translatedRows

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

        content = group [ gameBoard ]
    in
        collage collageWidth collageHeight [ content ]
            |> toHtml


textStyle : T.Text -> Form
textStyle = T.monospace
    >> T.height Const.fontHeight
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


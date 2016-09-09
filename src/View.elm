module View exposing (view)

import Collage exposing (collage, move, moveY, group, text)
import Element exposing (toHtml)
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

        styleFn = T.monospace >> T.height Const.fontHeight >> text
        styledRows =
            List.map styleFn rows

        translateFn r (i, rs) = (i + 1, (moveY (chHeight * i) r) :: rs)
        translatedRows = List.foldr translateFn (0, []) styledRows
            |> snd

        emptyGameBoard = group translatedRows

        playerOffset = Debug.log "offset" <| translateCoord model.player
        player = T.fromString "@"
            |> styleFn
            |> move playerOffset

        content = group [ emptyGameBoard, player ]
            |> moveY (-1 * gmHeight * chHeight / 2.0)
    in
        collage collageWidth collageHeight [ content ]
            |> toHtml


translateCoord : Model.Coordinate -> (Float, Float)
translateCoord {x, y} =
    let chWidth = toFloat Const.charWidth
        gmWidth = toFloat Const.gameWidth

        x' = chWidth * ((toFloat x) - gmWidth / 2.0) + Const.xCorrection
        y' = (y + 1) * Const.charHeight
    in
        (x', y')

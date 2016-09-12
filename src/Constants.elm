module Constants exposing (..)

windowWidth = 80
windowHeight = 25

gameWidth = 60
gameHeight = 23

charWidth = 10
charHeight = 18

fontSize = 16.0

xCorrection = 5.0

hudOffset = (32 * charWidth + 3, -10 * charHeight)

instructionHudWidth = 16

instructionHudLines =
    [ "Movement:"
    , ""
    , "y k u"
    , " \\|/"
    , "h- -l"
    , " /|\\"
    , "b j n"
    , ""
    , "Commands:"
    , ""
    , ".: stand still"
    , "t: teleport"
    , ""
    , "Legend:"
    , ""
    , "@: you"
    , "+: robot"
    , "*: junk"
    , ""
    ]


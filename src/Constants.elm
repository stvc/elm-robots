module Constants exposing (..)

windowWidth = 80
windowHeight = 25

gameWidth = 60
gameHeight = 23

charWidth = 10
charHeight = 18

fontHeight = 16.0

xCorrection = 5.0

hudOffset = (30 * charWidth + 3, -4 * charHeight)

instructionHudLines =
    [ "y k u         "
    , " \\|/          "
    , "h- -l         "
    , " /|\\          "
    , "b j n         "
    , ""
    , "Commands:     "
    , "              "
    , ".: stand still"
    , "t: teleport   "
    , ""
    , "Legend:       "
    , ""
    , "@: you        "
    , "+: robot      "
    , "*: junk       "
    ]

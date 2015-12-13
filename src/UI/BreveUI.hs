import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = startGUI defaultConfig {jsCustomHTML = Just "index.html", jsStatic = Just "static"} setup

setup :: Window -> UI ()
setup window = do
    return window # set UI.title "Script-n-Scribe"
    code <- codearea
    code2 <- codearea
    main <- UI.div #. "container" #+
        [ UI.div #. "header" #+ [string "Script-N-Scribe"]
        , UI.div #. "codebox" #+ [element code]
        , controls
        , UI.div #. "resbox" #+ [element code2]
        ]
    getBody window #+
        [ element main
        ]
    debug "Done"

codearea :: UI Element
codearea = do
    input <- UI.textarea #. "codearea"
    return input

controls :: UI Element
controls = do
    butt <- UI.button #. "button1" # set UI.text "SDKFJDS"
    cont <- (UI.div #. "controls") #+ [ element butt ]
    return cont

-- An aborted attempt to use ACE. Try again later.
-- main :: IO ()
-- main = do
--     startGUI defaultConfig {jsCustomHTML = Just "acepage.html"
--         , jsStatic = Just "static"} setup

-- setup :: Window -> UI ()
-- setup window = do
--     return window # set UI.title "Script-n-Scribe"
--     debug "DONE"


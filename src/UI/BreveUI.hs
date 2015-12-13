import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = startGUI defaultConfig {jsCustomHTML = Just "index.html", jsStatic = Just "static"} setup

setup :: Window -> UI ()
setup window = do
    return window # set UI.title "Script-n-Scribe"
    code <- codebox
    code2 <- codebox
    mainbox <- UI.div #. "container" #+
        [ element code
        , controls
        , element code2
        ]
    getBody window #+
        [ UI.div #. "header" #+ [string "Script-N-Scribe"]
        , element mainbox
        ]
    debug "Done"

-- For some reason threepenny doesn't have HTML attributes. We can make our own
-- pretty easily though!
wrap :: WriteAttr Element String
wrap = UI.mkWriteAttr (UI.set' (UI.attr "wrap"))

codebox :: UI Element
codebox = do
    input <- UI.textarea #. "codebox" # set wrap "off"
    return input

controls :: UI Element
controls = do
    butt <- UI.button #. "button1" # set UI.text "SDKFJDS"
    cont <- (UI.div #. "controls") #+ [ element butt ]
    return cont

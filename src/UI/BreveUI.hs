import qualified BreveEval

import Control.Monad.IO.Class (liftIO)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = startGUI defaultConfig {jsCustomHTML = Just "index.html", jsStatic = Just "static"} setup

setup :: Window -> UI ()
setup window = do
    return window # set UI.title "Script-n-Scribe"
    code <- codebox
    code2 <- codebox # set UI.value "Music goes here..."

    sync <- controlButton "Sync"
    play <- controlButton "Play"

    controls <- UI.div #. "controls" #+ map element [sync, play]

    mainbox <- UI.div #. "container" #+
        [ element code
        , element controls
        , element code2
        ]

    getBody window #+
        [ UI.div #. "header" #+ [string "Script-N-Scribe"]
        , element mainbox
        ]

    on UI.click sync $ const $ do
        source <- get UI.value code
        let (_,t) = BreveEval.parseEval source
        element code2 # set UI.value (unlines $ map show t)

    on UI.click play $ const $ do
        source <- get UI.value code
        liftIO (BreveEval.perform source)

codebox :: UI Element
codebox = do
    input <- UI.textarea #. "codebox" # set (UI.attr "wrap") "off"
    return input

controlButton :: String -> UI Element
controlButton name = UI.button #. "controlbutton" # set UI.text name

import qualified BreveEval
import qualified Synth

import Control.Monad.IO.Class (liftIO)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = startGUI defaultConfig {jsCustomHTML = Just "index.html", jsStatic = Just "static"} setup

setup :: Window -> UI ()
setup window = do
    return window # set UI.title "Script-n-Scribe"
    code <- codebox
    music <- codebox # set UI.value "Music goes here..."

    syncProg <- controlButton "Sync -->"
    syncMusic <- controlButton "<-- Sync"
    play <- controlButton "Play"

    controls <- UI.div #. "controls" #+ map element [syncProg, syncMusic, play]

    mainbox <- UI.div #. "container" #+
        [ element code
        , element controls
        , element music
        ]

    getBody window #+
        [ UI.div #. "header" #+ [string "Script-N-Scribe"]
        , element mainbox
        ]

    on UI.click syncProg $ const $ do
        source <- get UI.value code
        if null source then return syncProg else
            let (_,t) = BreveEval.parseEval source in
            element music # set UI.value (unlines $ map show t)

    on UI.click syncMusic $ const $ do
        source <- get UI.value code
        updates <- get UI.value music
        if null source || null updates then return syncMusic else
            let traces = Synth.toTraceMap $ snd $ BreveEval.parseEval source
                upTraces = map readVal $ lines updates
                subst = head $ Synth.synthFaithful traces upTraces in
            element code # set UI.value (show $ Synth.updateProgram subst (BreveEval.parse source))

    on UI.click play $ const $ do
        source <- get UI.value code
        liftIO (BreveEval.perform source)

codebox :: UI Element
codebox = do
    input <- UI.textarea #. "codebox" # set (UI.attr "wrap") "off"
    return input

controlButton :: String -> UI Element
controlButton name = UI.button #. "controlbutton" # set UI.text name

readVal :: String -> BreveEval.Val
readVal s = case words s of
    ("Pitch":p:t) -> BreveEval.Vp (read p) (read $ unwords t)
    ("Int":n:t) -> BreveEval.Vn (read n) (read $ unwords t)
    ("Double":d:t) -> BreveEval.Vd (read d) (read $ unwords t)

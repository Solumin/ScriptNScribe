import qualified BreveEval
import qualified Synth

import Control.Monad.IO.Class (liftIO)
import Data.List ((\\))
import Data.Maybe (fromJust)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = startGUI defaultConfig {jsCustomHTML = Just "index.html", jsStatic = Just "static"} setup

setup :: Window -> UI ()
setup window = do
    return window # set UI.title "Script-n-Scribe"
    codeBox <- codebox
    traceBox <- codebox # set UI.value "Music goes here..."
    snippetBox <- codebox # set UI.style [("display", "none")] # set (UI.attr "readonly") "readonly"

    syncProg <- controlButton "Sync -->"
    syncMusic <- controlButton "<-- Sync"
    play <- controlButton "Play"

    viewSnips <- controlButton "View Snippets"
    viewTraces <- controlButton "View Traces" # set UI.enabled False

    controls <- UI.div #. "controls" #+
        map element [syncProg, syncMusic, play, viewTraces, viewSnips]

    mainbox <- UI.div #. "container" #+
        [ element codeBox
        , element controls
        , element traceBox
        , element snippetBox
        ]

    getBody window #+
        [ UI.div #. "header" #+ [string "Script-N-Scribe"]
        , element mainbox
        ]

    on UI.click syncProg $ const $ do
        source <- get UI.value codeBox
        if null source then return syncProg else
            let (_,t) = BreveEval.parseEval source in
            element traceBox # set UI.value (unlines $ map show t)

    on UI.click syncMusic $ const $ do
        source <- get UI.value codeBox
        updates <- get UI.value traceBox
        if null source || null updates then return syncMusic else
            let traces = snd $ BreveEval.parseEval source
                upTraces = (map readVal $ lines updates)
                subst = fromJust $ Synth.synthFaithfulLive traces upTraces in
            element codeBox # set UI.value (show $ Synth.updateProgram subst (BreveEval.parse source))

    on UI.click play $ const $ do
        source <- get UI.value codeBox
        liftIO (BreveEval.perform source)

    on UI.click viewTraces $ const $ do
        source <- get UI.value codeBox
        if null source then return codeBox
        -- no traces to get, still swap to window
        else
            let traces = snd $ BreveEval.parseEval source in
            element traceBox # set UI.value (unlines $ map show traces)
        -- switch out right-side boxes
        element traceBox # set UI.style [("display", "inline-block")]
        element snippetBox # set UI.style [("display", "none")]
        -- disable this button
        element viewTraces # set UI.enabled False
        -- enable the other two
        element viewSnips # set UI.enabled True
        -- element viewMusic # set UI.enabled True

    on UI.click viewSnips $ const $ do
        -- get snippets
        source <- get UI.value codeBox
        let snippets = getSnippets $ fst $ BreveEval.parseEval source
        -- show snippets
        element snippetBox # set UI.value (unlines $ map showSnippet snippets)
        -- switch out right-side boxes
        element snippetBox # set UI.style [("display", "inline-block")]
        element traceBox # set UI.style [("display", "none")]
        -- disable this button
        element viewSnips # set UI.enabled False
        -- enable the other two
        element viewTraces # set UI.enabled True
        -- element viewMusic # set UI.enabled True

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

getSnippets :: BreveEval.EvalRes -> BreveEval.EvalRes
getSnippets = filter isMusic
    where isMusic (_, v) =  case v of
            (BreveEval.Vnote{}) -> True
            (BreveEval.Vrest{}) -> True
            (BreveEval.Vpar{}) -> True
            (BreveEval.Vseq{}) -> True
            _ -> False

showSnippet :: BreveEval.Binding -> String
showSnippet (s, v) = s ++ ": " ++ showSnipVal v

showSnipVal :: BreveEval.Val -> String
showSnipVal v = case v of
    (BreveEval.Vnote (BreveEval.Vp p _) o d) -> '(' : shows p (' ' : showsNum o (' ' : showsNum d ")"))
    (BreveEval.Vrest d) -> "(rest " ++ showsNum d ")"
    (BreveEval.Vseq a b) -> '(': showSnipVal a ++ ") :+: (" ++ showSnipVal b ++ ")"
    (BreveEval.Vpar a b) -> '(': showSnipVal a ++ ") :=: (" ++ showSnipVal b ++ ")"

showsNum :: BreveEval.Val -> (String -> String)
showsNum (BreveEval.Vn n _) = shows n
showsNum (BreveEval.Vd d _) = shows d

module Main where

import qualified Graphics.UI.Gtk.Builder as B
import qualified Graphics.UI.Gtk as G

import qualified Graphics.Rendering.Cairo as C

import Debug.Trace (traceM)

import Waveform

import qualified SubtitleList as S (empty)

import PeakExtractor
import AudioInfo

zinfo :: ZoomInfo
zinfo = defaultZoomInfo

drawPath :: AudioInfo -> G.DrawingArea -> C.Render ()
drawPath af = waveformPainter basicColorConfig af S.empty zinfo

main :: IO ()
main = do
    res <- readFromDump "/home/francesco/Desktop/PeakFile"
    
    G.initGUI
    --builder <- B.builderNew
    --B.builderAddFromFile builder "GtkGui.glade"

    --mainWindow <- B.builderGetObject builder G.castToWindow "mainWindow"
    --canvas     <- B.builderGetObject builder G.castToDrawingArea "timeline"

    mainWindow <- G.windowNew
    canvas <- G.drawingAreaNew

    G.containerAdd mainWindow canvas

    G.widgetSetSizeRequest canvas 938 266

    mainWindow `G.on` G.objectDestroy $ G.mainQuit
    canvas `G.on` G.draw $ drawPath res canvas

    G.widgetShowAll mainWindow
    G.mainGUI

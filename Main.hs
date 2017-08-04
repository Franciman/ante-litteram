module Main where

import qualified Graphics.UI.Gtk.Builder as B
import qualified Graphics.UI.Gtk as G

main :: IO ()
main = do
    G.initGUI
    builder <- B.builderNew
    B.builderAddFromFile builder "GtkGui.glade"

    mainWindow <- B.builderGetObject builder G.castToWindow "mainWindow"

    mainWindow `G.on` G.objectDestroy $ G.mainQuit

    G.widgetShowAll mainWindow
    G.mainGUI

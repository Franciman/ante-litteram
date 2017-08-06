module Main where

import PeakExtractor
import AudioInfo
import Waveform
import System.TimeIt

import Graphics.UI.WX

import SubtitleList
import SubtitleParser
import SRTParser

import qualified Data.Text.IO as T

printProgress :: ProgressTracker
printProgress curr total = do
    let percent = round ((fromIntegral curr / fromIntegral total) * 100) :: Int
    putStrLn (show percent ++ "%")

emptyProg :: ProgressTracker
emptyProg _ _ = return ()

gui :: AudioInfo -> SubtitleList -> IO ()
gui res subs = do
    f <- frame [text := "Waveform"]
    p <- panel f []
    w <- newWaveform res subs p
    set (waveformWidget w) [clientSize := (sz 938 266)]
    --set f [layout := rigid (widget $ waveformWidget w)]

main :: IO ()
main = do
    --res <- extractPeaks emptyProg "/home/francesco/Desktop/vid.mp4"
    --dumpInfos res "/home/francesco/Desktop/PeakFileHs"
    res <- Just <$> readFromDump "/home/francesco/Desktop/PeakFile"

    (Right subs) <- (flip parseSubs) srtParser <$> T.readFile "/home/francesco/Desktop/VO.srt"


    let subsList = fromList subs
    putStrLn $ "Len: " ++ (show (length subs))
    putStrLn $ "Size: " ++ (show (SubtitleList.size subsList))
    case res of
        Nothing -> putStrLn "Error while reading media file"
        Just res -> start (gui res subsList)

    --timedExtraction emptyProg "/home/francesco/Desktop/vid.mp4"

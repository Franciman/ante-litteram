module AudioInfo where

import Peak

import Data.Int
import Debug.Trace
import System.IO

import qualified Data.Vector as V



data AudioInfo = AudioInfo
               { peaks          :: V.Vector Peak
               , sampleRate     :: Int
               , samplesPerPeak :: Int
               }

peaksPerSecond :: AudioInfo -> Double
peaksPerSecond (AudioInfo _ sampleRate samplesPerPeak) = let sr = realToFrac sampleRate
                                                             spp = realToFrac samplesPerPeak
                                                         in sr / spp

dumpPeaks :: (Foldable f) => Handle -> f Peak -> IO ()
dumpPeaks h = mapM_ (\(Peak mv mV) -> hPutStrLn h (show mv) >> hPutStrLn h (show mV))

dumpInfos :: AudioInfo -> String -> IO ()
dumpInfos (AudioInfo ps sr spp) f = withFile f WriteMode $ \h -> do
    hPutStrLn h . show $ sr
    hPutStrLn h . show $ spp
    dumpPeaks h ps

readRecord :: Handle -> IO Peak
readRecord h = do
    minP <- read <$> hGetLine h :: IO Int16
    maxP <- read <$> hGetLine h :: IO Int16
    return (Peak minP maxP)

readRecords :: Handle -> IO [Peak]
readRecords h = do
    eof <- hIsEOF h
    if not eof
    then do
        p <- readRecord h
        (p :) <$> readRecords h
    else return []

readFromDump :: String -> IO AudioInfo
readFromDump f = withFile f ReadMode $ \h -> do
    sampleRate <- read <$> hGetLine h :: IO Int
    samplesPerPeak <- read <$> hGetLine h :: IO Int
    peaks <- readRecords h

    return (AudioInfo (V.fromList peaks) sampleRate samplesPerPeak)

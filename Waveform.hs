module Waveform where

import Graphics.UI.WX

import Peak
import AudioInfo

import Data.List
import qualified Data.Vector as V

import Control.Arrow ((***), first)
import Control.Monad (forM_)

import Data.Int (Int16)

import Debug.Trace

import System.IO

import Graphics.UI.WXCore.Draw (getTextExtent)
import Graphics.UI.WXCore.WxcTypes (colorRGBA)

import Text.Printf

import SubtitleList
import Subtitle

import Data.Maybe (fromMaybe)


round' :: (RealFrac a, Integral b) => a -> b
round' x = let (n, r) = properFraction x
               m      = if r < 0 then n - 1 else n + 1
           in case signum (abs r - 0.5) of
               -1 -> n
               0  -> m -- This is where round' differs from round, it doesn't return the even value, but the greater
               1  -> m
               _  -> error "round': Bad value"

data ZoomInfo = ZoomInfo
          { pageSizeMs :: Int
          , verticalScaling :: Int
          , positionMs :: Int
          }

positionIsVisible :: ZoomInfo -> Int -> Bool
positionIsVisible info posMs = let start = positionMs info
                                   end   = start + pageSizeMs info
                               in start <= posMs && posMs <= end

data Zoom = Zoom ZoomInfo Rect

startPos :: Zoom -> Int
startPos (Zoom info _) = positionMs info

msPerPixel :: Zoom -> Double
msPerPixel (Zoom info bounds) = let width    = realToFrac (rectWidth bounds)
                                    pageSize = realToFrac (pageSizeMs info)
                                in pageSize / width

timeToPixel :: Zoom -> Int -> Int
timeToPixel ctx@(Zoom info r) posMs = let dt = posMs - positionMs info
                                      in round' (realToFrac dt / msPerPixel ctx)

scalePeakValue :: Zoom -> Int16 -> Int16
scalePeakValue (Zoom info bounds) p = let vs = verticalScaling info
                                          height = realToFrac (rectHeight bounds) :: Double
                                          sp = realToFrac ((fromIntegral p) * vs) / 100 :: Double
                                          sp' = (sp * height) / 65536
                                      in round' sp'

defaultZoomInfo :: ZoomInfo
defaultZoomInfo = ZoomInfo 15000 100 0


zoomPeaks :: Int -> Int -> AudioInfo -> Zoom -> [Peak]
zoomPeaks from to audioInfo zoomCtx = let peaksPerMs           = (peaksPerSecond audioInfo) / 1000
                                          peaksPerPixel        = (peaksPerMs * (msPerPixel zoomCtx))
                                          peaksPerPixelRounded = round' peaksPerPixel
                                          startPosMs           = realToFrac (startPos zoomCtx)
                                          startPeak            = round' (peaksPerMs * startPosMs)
                                          peakList             = (peaks audioInfo)


                                          peakGroups :: [V.Vector Peak]
                                          peakGroups = map groupPeaks [from..to]

                                          groupPeaks :: Int -> V.Vector Peak
                                          groupPeaks pixel = let firstPeak = round' (realToFrac pixel * peaksPerPixel) + startPeak
                                                             in V.take peaksPerPixelRounded . V.drop firstPeak $ peakList

                                          mkPeak :: V.Vector Peak -> Peak
                                          mkPeak = trace ("PeaksPerPixel: " ++ show peaksPerPixel) $ V.foldr1 fusePeaks

                                          fusePeaks :: Peak -> Peak -> Peak
                                          fusePeaks (Peak lMin lMax) (Peak rMin rMax) =
                                              Peak (min lMin rMin) (max lMax rMax)

                                          scalePeak :: Peak -> Peak
                                          scalePeak (Peak pMin pMax) = let scaledMin = scalePeakValue zoomCtx pMin
                                                                           scaledMax = scalePeakValue zoomCtx pMax
                                                                     in Peak scaledMin scaledMax

                                      in trace ("PpP: " ++ (show peaksPerPixel) ++ " PpPR: " ++ (show peaksPerPixelRounded)) $ map (scalePeak . mkPeak) peakGroups


data ColorConfig = ColorConfig
                 { waveBackColor           :: Color
                 , waveColor               :: Color
                 , rangeColor1             :: Color
                 , rangeColor2             :: Color
                 , nonEditableRangeColor   :: Color
                 , selectionColor          :: Color
                 , minBlankColor           :: Color
                 , cursorColor             :: Color
                 , rulerBackColor          :: Color
                 , rulerTopBottomLineColor :: Color
                 , rulerTextColor          :: Color
                 , rulerTextShadowColor    :: Color
                 }

basicColorConfig :: ColorConfig
basicColorConfig = ColorConfig
                 { waveBackColor           = colorRGB 11 19 43
                 , waveColor               = colorRGB 111 255 233
                 , rangeColor1             = colorRGB 62 120 178
                 , rangeColor2             = colorRGB 241 136 5
                 , nonEditableRangeColor   = colorRGB 141 153 174
                 , selectionColor          = colorRGBA 255 255 255 20
                 , minBlankColor           = colorRGBA 255 255 255 90
                 , cursorColor             = colorRGB 74 49 77
                 , rulerBackColor          = colorRGB 81 71 65
                 , rulerTopBottomLineColor = colorRGB 190 181 174
                 , rulerTextColor          = colorRGB 224 224 224
                 , rulerTextShadowColor    = colorRGB 0 0 0
                 }

data Waveform = Waveform
              { zoomInfos :: ZoomInfo
              , widgetW :: Panel ()
              }

newWaveform :: ColorConfig -> AudioInfo -> SubtitleList -> Window a -> IO Waveform
newWaveform colors audioInfo subs parent = Waveform defaultZoomInfo <$>
                                        panel parent [on paint := paintWaveform colors audioInfo subs defaultZoomInfo]

waveformWidget :: Waveform -> Panel ()
waveformWidget = widgetW

paintWaveform :: ColorConfig -> AudioInfo -> SubtitleList -> ZoomInfo -> DC () -> Rect -> IO ()
paintWaveform colors audioInfo subs zoomInfo dc bounds = do
    let rulerHeight = 20
    let waveRect = bounds { rectHeight = rectHeight bounds - rulerHeight }
    paintWave colors audioInfo zoomInfo dc waveRect

    let rulerRect = bounds { rectTop = (rectHeight bounds) - rulerHeight, rectHeight = rulerHeight }
    paintRuler colors zoomInfo dc rulerRect

    paintSubs colors dc subs zoomInfo waveRect

paintWave :: ColorConfig -> AudioInfo -> ZoomInfo -> DC () -> Rect -> IO ()
paintWave colors audioInfo zoomInfo dc bounds = do
    let zoomCtx = Zoom zoomInfo bounds
    let width = rectWidth bounds - 1
    let zoomedPeaks = zoomPeaks 0 width  audioInfo zoomCtx
    let zoomedPeaksWithPos = zip [0 .. width] zoomedPeaks
    let middlePos = traceShow (rectHeight bounds) $ (rectHeight bounds) `quot` 2

    withFile "/home/francesco/ScaledPeaksHs" WriteMode $ (flip dumpPeaks) (map snd zoomedPeaksWithPos)
    trace ("Width: " ++ (show (rectWidth bounds))) $ return ()
    trace ("Height: " ++ (show (rectHeight bounds))) $ return ()
    trace ("Peaks: " ++ (show (length zoomedPeaks))) $ return ()
   
    drawRect dc bounds [brushColor := waveBackColor colors]

    set dc [penColor := waveColor colors]

    forM_ zoomedPeaksWithPos $ \(pos, peak) -> do
        let from = point pos (middlePos - fromIntegral (maxVal peak))
        let to   = point pos (middlePos - fromIntegral (minVal peak))
        line dc from to []

    let from = point 0 middlePos
    let to   = point (rectWidth bounds) middlePos
    line dc from to []

paintRuler :: ColorConfig -> ZoomInfo -> DC () -> Rect -> IO ()
paintRuler colors zoomInfo dc bounds = do
    drawRect dc bounds [brushColor := rulerBackColor colors]

    set dc [penColor := rulerTopBottomLineColor colors]
    line dc (rectTopLeft bounds) (rectTopRight bounds) []
    line dc (rectBottomLeft bounds) (rectBottomRight bounds) []

    let startTime = positionMs zoomInfo
    let endTime   = startTime + pageSizeMs zoomInfo

    let zoom = Zoom zoomInfo bounds

    set dc [fontFace := "Time New Roman", fontSize := 8]

    textSize <- getTextExtent dc "0:00:00.0"

    trace ("Text size: " ++ show textSize) $ return ()

    let textWidth = sizeW textSize * 2
    let textHeight = sizeH textSize

    let maxStep = round' (realToFrac (rectWidth bounds) / realToFrac textWidth)
    let stepMs' = round' (realToFrac (pageSizeMs zoomInfo) / realToFrac maxStep)
    let stepMs''  = (if stepMs' == 0 then 1 else realToFrac stepMs') :: Double

    -- Find the power of 10 that best approximates (from below) stepMs
    let exponent = truncate (logBase 10 stepMs'')
    let stepApprox = truncate $ 10 ** (realToFrac exponent)

    let stepMs = (truncate (stepMs'' / realToFrac stepApprox)) * stepApprox
    let p      = (startTime `quot` stepMs) * stepMs

    let positions = takeWhile (\p' -> p' < startTime + pageSizeMs zoomInfo) $
            map (\m -> p + m * stepMs) [0..]

    putStrLn $ "Positions: " ++ (show positions)

    let makeParams = (\pos -> let pixelPerMs = ( 1 / msPerPixel zoom)
                                  x = truncate $ realToFrac (pos - startTime) * pixelPerMs
                                  x2 = x + (stepMs `quot` 2) * (truncate pixelPerMs)
                                  timing = timeMsToString pos stepApprox exponent
                              in (timing, x, x2))

    mapM_ ((\(timing, x, x2) -> drawTime colors dc bounds timing x x2) . makeParams) positions


timeMsToString :: Int -> Int -> Int -> String
timeMsToString timeMs msPrecision msPrecisionLog = formatString . uncurry (:) $ foldr extractUnit (timeMs, []) [60, 60, 1000]
    where extractUnit multiplier (remainder, us) = let u = remainder `mod` multiplier
                                                   in ((remainder - u) `quot` multiplier, u : us)

          formatString :: [Int] -> String
          formatString [h, m, s, ms] = let res = printf "%d:%02d:%02d" h m s
                                       in if ms > 0
                                          then res ++ printf (".%0" ++ show (3 - msPrecisionLog) ++ "d") ms
                                          else res

drawTime :: ColorConfig -> DC () -> Rect -> String -> Int -> Int -> IO ()
drawTime colors dc bounds timing x x2 = do

    -- Draw main division
    let from = Point x $ (rectTop bounds) + 1
    let to   = Point x $ (rectTop bounds) + 5
    line dc from to [penColor := rulerTextColor colors]


    textSize <- getTextExtent dc timing

    let x' = x - ((sizeW textSize) `quot` 2)
    let y' = (rectTop bounds) + {-(sizeH textSize) +-} 4


    -- Draw text shadow
    drawText dc timing (Point (x' + 2) (y' + 2)) [textColor := rulerTextShadowColor colors]
    -- Draw text
    drawText dc timing (Point x' y') [textColor := rulerTextColor colors]

    -- Draw subdivision
    let from2 = Point x2 $ (rectTop bounds) + 1
    let to2   = Point x2 $ (rectTop bounds) + 3
    line dc from2 to2 [penColor := rulerTextColor colors]




paintSubs :: ColorConfig -> DC () -> SubtitleList -> ZoomInfo -> Rect -> IO ()
paintSubs colorC dc subs zoomInfo bounds = do
    let colors = [rangeColor2 colorC, rangeColor1 colorC]

    let heightDiv10 = (rectHeight bounds) `quot` 10
    let y1 = (rectTop bounds) + heightDiv10
    let y2 = (rectBottom bounds) - heightDiv10

    let startTime = positionMs zoomInfo
    let endTime   = startTime + pageSizeMs zoomInfo

    let visibleSubs = subsOverlappingIntervalWithIndex (TimeInterval startTime endTime) subs

    let subsAndColors = map (first (\i -> colors !! mod i 2)) visibleSubs

    let zoomCtx = Zoom zoomInfo bounds

    forM_ subsAndColors $ \(color, sub) -> do
        set dc [penColor := color, textColor := color]
        paintSub dc sub zoomCtx y1 y2


paintSub :: DC () -> Subtitle -> Zoom -> Int -> Int -> IO ()
paintSub dc sub zoomCtx@(Zoom info bounds) y1 y2 = do

        subStartPx <- fromMaybe 0 <$> drawTimePoint (low . timeInterval $ sub)
        subEndPx   <- fromMaybe (rectWidth bounds) <$> drawTimePoint (high . timeInterval $ sub)

        line dc (Point subStartPx y1) (Point subEndPx y1) []
        line dc (Point subStartPx y2) (Point subEndPx y2) []

        let textMargins = 5
        let minSpace    = 25
        when (subEndPx - subStartPx - 2 * textMargins > minSpace) $ do

            let subStartPx' = subStartPx + textMargins
            --let subEndPx'   = subEndPx - textMargins

            drawText dc (Subtitle.dialog sub) (Point subStartPx' y1) []--[textColor := red]
        
    where 
          drawTimePoint :: Int -> IO (Maybe Int)
          drawTimePoint posMs = do
             if not (positionIsVisible info posMs)
             then return Nothing
             else do
                 let timePx = timeToPixel zoomCtx posMs
                 let from = Point timePx (rectTop bounds)
                 let to   = Point timePx (rectBottom bounds)
                 line dc from to [penKind := PenDash DashDot]
                 return (Just timePx)


module Waveform where

import Peak
import AudioInfo

import Data.List
import qualified Data.Vector as V

import Control.Arrow ((***), first)
import Control.Monad (forM_)

import Data.Int (Int16)

import Debug.Trace

import System.IO

import Text.Printf

import SubtitleList
import Subtitle

import Data.Maybe (fromMaybe)

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.UI.Gtk as G


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

data Zoom = Zoom ZoomInfo Int Int

startPos :: Zoom -> Int
startPos (Zoom info _ _) = positionMs info

msPerPixel :: Zoom -> Double
msPerPixel (Zoom info w _) = let width    = realToFrac w
                                 pageSize  = realToFrac (pageSizeMs info)
                             in pageSize / width

timeToPixel :: Zoom -> Int -> Int
timeToPixel ctx@(Zoom info _ _) posMs = let dt = posMs - positionMs info
                                        in round' (realToFrac dt / msPerPixel ctx)

scalePeakValue :: Zoom -> Int16 -> Int16
scalePeakValue (Zoom info _ h) p = let vs = verticalScaling info
                                       height = realToFrac h :: Double
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





--data Waveform = Waveform
              --{ zoomInfos :: ZoomInfo
              --, widgetW :: Panel ()
              --}

--newWaveform :: AudioInfo -> SubtitleList -> Window a -> IO Waveform
--newWaveform audioInfo subs parent = Waveform defaultZoomInfo <$>
                                        --panel parent [on paint := paintWaveform audioInfo subs defaultZoomInfo]


--paintWaveform :: AudioInfo -> SubtitleList -> ZoomInfo -> DC () -> Rect -> IO ()
--paintWaveform audioInfo subs zoomInfo dc bounds = do
    --let rulerHeight = 20
    --let waveRect = bounds { rectHeight = rectHeight bounds - rulerHeight }
    --paintWave audioInfo zoomInfo dc waveRect

    --let rulerRect = bounds { rectTop = (rectHeight bounds) - rulerHeight, rectHeight = rulerHeight }
    --paintRuler zoomInfo dc rulerRect

    --paintSubs dc subs zoomInfo waveRect

data Color = Color { red :: Double, green :: Double, blue :: Double, alpha :: Double }

color :: Int -> Int -> Int -> Color
color r g b = Color (normalize r) (normalize g) (normalize b) 1
    where normalize v = realToFrac v / 255

colorA :: Int -> Int -> Int -> Int -> Color
colorA r g b a = Color (normalize r) (normalize g) (normalize b) (normalizeA a)
    where normalize v = realToFrac v / 255
          normalizeA v = realToFrac v / 100

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
                 { waveBackColor           = color 11 19 43
                 , waveColor               = color 111 255 233
                 , rangeColor1             = color 62 120 178
                 , rangeColor2             = color 241 136 5
                 , nonEditableRangeColor   = color 141 153 174
                 , selectionColor          = colorA 255 255 255 20
                 , minBlankColor           = colorA 255 255 255 90
                 , cursorColor             = color 74 49 77
                 , rulerBackColor          = color 81 71 65
                 , rulerTopBottomLineColor = color 190 181 174
                 , rulerTextColor          = color 224 224 224
                 , rulerTextShadowColor    = color 0 0 0
                 }

setSourceColor :: Color -> C.Render ()
setSourceColor (Color r g b a) = C.setSourceRGBA r g b a

waveformPainter :: ColorConfig -> AudioInfo -> SubtitleList -> ZoomInfo -> G.DrawingArea -> C.Render ()
waveformPainter colors audioInfo subs zoomInfo drawArea = do
    allocation@(G.Rectangle x y width height) <- C.liftIO $ G.widgetGetAllocation drawArea
    let rulerHeight = 20

    let waveAlloc = G.Rectangle x y width (height - rulerHeight)

    wavePainter colors audioInfo zoomInfo waveAlloc

    let rulerAlloc = G.Rectangle x (height - rulerHeight) width rulerHeight
    rulerPainter colors zoomInfo rulerAlloc




drawLine :: (Int, Int) -> (Int, Int) -> C.Render ()
drawLine from to = do
    let extract = (realToFrac .)
    C.moveTo (extract fst from) (extract snd from)
    C.lineTo (extract fst to) (extract snd to)

wavePainter :: ColorConfig -> AudioInfo -> ZoomInfo -> G.Allocation -> C.Render ()
wavePainter colors audioInfo zoomInfo (G.Rectangle x y width height) = do
    let zoomCtx = Zoom zoomInfo width height
    let zoomedPeaksWithPos = zip [0 .. width] $ zoomPeaks 0 width audioInfo zoomCtx
    let middlePos = traceShow height $ height `quot` 2

    setSourceColor (waveBackColor colors)
    C.rectangle (realToFrac x) (realToFrac y) (realToFrac width) (realToFrac height)
    C.fill

    lw <- C.getLineWidth

    traceM ("Line width: " ++ show lw)

   
    setSourceColor (waveColor colors)

    forM_ zoomedPeaksWithPos $ \(pos, peak) -> do
       let from = (pos, middlePos - fromIntegral (maxVal peak))
       let to   = (pos, middlePos - fromIntegral (minVal peak))
       drawLine from to

    drawLine (0, middlePos) (width, middlePos)
    C.stroke

rulerPainter :: ColorConfig -> ZoomInfo -> G.Allocation -> C.Render ()
rulerPainter colors zoomInfo alloc@(G.Rectangle x y width height) = do
    let startTime = positionMs zoomInfo
    let endTime   = startTime + pageSizeMs zoomInfo
    let zoom      = Zoom zoomInfo width height


    setSourceColor (rulerBackColor colors)
    C.rectangle (realToFrac x) (realToFrac y) (realToFrac width) (realToFrac height)
    C.fill

    setSourceColor (rulerTopBottomLineColor colors)
    drawLine (x, y) (x + width, y)
    drawLine (x, y + height) (x + width, y + height)
    C.stroke


    setSourceColor (rulerTextColor colors)

    C.selectFontFace "Time New Roman" C.FontSlantNormal C.FontWeightNormal
    C.setFontSize 12.0

    textSize <- C.textExtents "0:00:00.0"

    let textWidth  = truncate (C.textExtentsWidth textSize * 2)
    let textHeight = truncate (C.textExtentsHeight textSize)

    let maxStep = round' (realToFrac width / realToFrac textWidth)
    let stepMs' = round' (realToFrac (pageSizeMs zoomInfo) / realToFrac maxStep)
    let stepMs'' = (if stepMs' == 0 then 1 else realToFrac stepMs') :: Double

    -- Find the power of 10 that best approximates (from below) stepMs
    let exponent = truncate (logBase 10 stepMs'')
    let stepApprox = truncate $ 10 ** (realToFrac exponent)

    let stepMs = (truncate (stepMs'' / realToFrac stepApprox)) * stepApprox
    let p      = (startTime `quot` stepMs) * stepMs

    let positions = takeWhile (\p' -> p' < startTime + pageSizeMs zoomInfo) $ map (\m -> p + m * stepMs) [0..]

    let makeParams = (\pos -> let pixelPerMs = 1 / msPerPixel zoom
                                  x = truncate $ realToFrac (pos - startTime) * pixelPerMs
                                  x2 = x + (stepMs `quot` 2) * (truncate pixelPerMs)
                                  timing = timeMsToString pos stepApprox exponent
                              in (timing, x, x2))

    mapM_ ((\(timing, x, x2) -> drawTime colors alloc timing x x2) . makeParams) positions

        
timeMsToString :: Int -> Int -> Int -> String
timeMsToString timeMs msPrecision msPrecisionLog = formatString . uncurry (:) $ foldr extractUnit (timeMs, []) [60, 60, 1000]
    where extractUnit multiplier (remainder, us) = let u = remainder `mod` multiplier
                                                   in ((remainder - u) `quot` multiplier, u : us)

          formatString :: [Int] -> String
          formatString [h, m, s, ms] = let res = printf "%d:%02d:%02d" h m s
                                       in if ms > 0
                                          then res ++ printf (".%0" ++ show (3 - msPrecisionLog) ++ "d") ms
                                          else res


drawTime :: ColorConfig -> G.Allocation -> String -> Int -> Int -> C.Render ()
drawTime colors (G.Rectangle left top width height) timing x x2 = do
    -- Draw main division
    let from = (x, top + 1)
    let to   = (x, top + 5)

    drawLine from to
    C.stroke

    textSize <- C.textExtents timing

    let x' = x - (truncate (C.textExtentsWidth textSize) `quot` 2)
    let y' = top + 4 + truncate (C.textExtentsHeight textSize)

    -- Draw text shadow
    setSourceColor (rulerTextShadowColor colors)
    C.moveTo (realToFrac x' + 2) (realToFrac y' + 2)
    C.showText timing

    -- Draw text
    setSourceColor (rulerTextColor colors)
    C.setSourceRGB 255 255 255
    C.moveTo (realToFrac x') (realToFrac y')
    C.showText timing

    -- Draw subdivision
    drawLine (x2, top + 1) (x2, top + 3)
    C.stroke


--paintSubs :: DC () -> SubtitleList -> ZoomInfo -> Rect -> IO ()
--paintSubs dc subs zoomInfo bounds = do
    --let colors = [blue, magenta]

    --let heightDiv10 = (rectHeight bounds) `quot` 10
    --let y1 = (rectTop bounds) + heightDiv10
    --let y2 = (rectBottom bounds) - heightDiv10

    --let startTime = positionMs zoomInfo
    --let endTime   = startTime + pageSizeMs zoomInfo

    --let visibleSubs = subsOverlappingIntervalWithIndex (TimeInterval startTime endTime) subs

    --let subsAndColors = map (first (\i -> colors !! mod i 2)) visibleSubs

    --let zoomCtx = Zoom zoomInfo bounds

    --forM_ subsAndColors $ \(color, sub) -> do
        --set dc [penColor := color]
        --paintSub dc sub zoomCtx y1 y2


--paintSub :: DC () -> Subtitle -> Zoom -> Int -> Int -> IO ()
--paintSub dc sub zoomCtx@(Zoom info bounds) y1 y2 = do

        --subStartPx <- fromMaybe 0 <$> drawTimePoint (low . timeInterval $ sub)
        --subEndPx   <- fromMaybe (rectWidth bounds) <$> drawTimePoint (high . timeInterval $ sub)

        --line dc (Point subStartPx y1) (Point subEndPx y1) []
        --line dc (Point subStartPx y2) (Point subEndPx y2) []

        --let textMargins = 5
        --let minSpace    = 25
        --when (subEndPx - subStartPx - 2 * textMargins > minSpace) $ do

            --let subStartPx' = subStartPx + textMargins
            --let subEndPx'   = subEndPx - textMargins

            --drawText dc (Subtitle.dialog sub) (Point subStartPx' y1) [textColor := red]
        
    --where 
          --drawTimePoint :: Int -> IO (Maybe Int)
          --drawTimePoint posMs = do
             --if not (positionIsVisible info posMs)
             --then return Nothing
             --else do
                 --let timePx = timeToPixel zoomCtx posMs
                 --let from = Point timePx (rectTop bounds)
                 --let to   = Point timePx (rectBottom bounds)
                 --line dc from to [penKind := PenDash DashDot]
                 --return (Just timePx)


module SRTParser where

import qualified Text.Subtitles.SRT as SRT
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A

import SubtitleParser
import Subtitle

instance SubtitleLike SRT.Line where
    dialog = T.unpack . SRT.dialog
    timeInterval s = TimeInterval start end
        where range = SRT.range s
              start = toMs (SRT.from range)
              end   = toMs (SRT.to range)

              toMs (SRT.Time h m s ms) = (h * 3600 + m * 60 + s) * 1000 + ms

srtParser :: T.Text -> Either String SRT.Subtitles
srtParser = A.parseOnly SRT.parseSRT

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module SubtitleParser where
    
import Subtitle hiding (dialog, timeInterval)
import qualified Subtitle as S

import qualified Data.Text as T

class SubtitleLike a where
    dialog :: a -> String
    timeInterval :: a -> TimeInterval

    makeSubtitle :: a -> Subtitle
    makeSubtitle s = Subtitle (timeInterval s) (dialog s)

instance SubtitleLike Subtitle where
    dialog = S.dialog
    timeInterval = S.timeInterval
    -- No need to convert
    makeSubtitle = id


parseSubs :: (Show e, SubtitleLike a) => T.Text -> (T.Text -> Either e [a]) -> Either String [Subtitle]
parseSubs t f = let result = f t
                in either (Left . show) (Right . (map makeSubtitle)) result

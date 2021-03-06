module HIF.Tools where

import Control.Lens
import Data.Maybe

-- Unsafely lens into a Maybe
(^.?) :: Show s => s -> Getting (Maybe a) s (Maybe a) -> a
a ^.? b = fromMaybe (error $ "Unsafe entity attribute access" ++ show a) (a ^. b)

-- Conditional string
cT :: Bool -> a -> Maybe a
cT b a = if b then Just a else Nothing

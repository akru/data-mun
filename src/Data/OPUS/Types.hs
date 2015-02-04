module Data.OPUS.Types where

import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)

-- | Language string: en, fr, ru, etc.
type Lang         = String
-- | Parallel text map by language name
type ParallelText = Map Lang Text
-- | Set of parallel texts
type Corpus       = Set ParallelText


module Data.OPUS.Types where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

-- | Language string: en, fr, ru, etc.
type Lang         = String
-- | Parallel text map by language name
type ParallelText = Map Lang Text
-- | Set of parallel texts
type Corpus       = Set ParallelText

-- | Makes Corpus from list of paralles texts.
--   Parallel text presented as list of pairs: language name - text.
mkCorpus :: [[(Lang, Text)]] -> Corpus
mkCorpus = Set.fromList . map Map.fromList


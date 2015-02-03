module Data.OPUS.Types where

import Data.Set (Set)
import Data.Set as Set
import Data.Map (Map)
import Data.Map as Map

-- | Language string: en, fr, ru, etc.
type Lang         = String
-- | Parallel text map by language name
type ParallelText = Map Lang String
-- | Set of parallel texts
type Corpus       = Set ParallelText

-- | Makes Corpus from list of paralles texts.
--   Parallel text presented as list of pairs: language name - text.
mkCorpus :: [[(Lang, String)]] -> Corpus
mkCorpus = Set.fromList . Prelude.map Map.fromList


module Data.OPUS.Types where

import Data.Set (Set)
import Data.Map (Map)
import Data.Binary

type Lang         = String
type Corpus       = Set ParallelText
type ParallelText = Map Lang String

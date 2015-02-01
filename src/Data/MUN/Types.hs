module Data.MUN.Types where

import Data.IntMap.Strict
import Data.Map.Strict

type Corpus    = IntMap String
type Lang      = String
type ParCorpus = Map Lang Corpus


module Data.OPUS.Types where

import Data.Map

type Lang      = String
type Corpus    = Map Int String
type ParCorpus = Map Lang Corpus


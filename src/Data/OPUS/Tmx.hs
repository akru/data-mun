module Data.OPUS.Tmx where

import Data.OPUS.Types
import Data.OPUS.Loader

import Control.Monad (liftM)
import Text.XML.HXT.Core

parseTmx :: String -> IO Corpus
parseTmx fileName = liftM mkCorpus . runX $
    readDocument [withValidate no] fileName >>> tmxParser

tmxParser :: ArrowXml a => a XmlTree [(Lang, String)]
tmxParser = getChildren >>>
    hasName "tmx" //> hasName "tu" >>> listA value
  where
    value = getChildren >>> hasName "tuv"
        &&& getAttrValue "xml:lang"
        >>> first (getChildren >>> hasName "seg" >>> getChildren >>> getText)
        >>^ swap


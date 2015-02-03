module Data.OPUS.Tmx (parseTmx) where

import Data.OPUS.Types
import Data.OPUS.Loader

import Text.XML.HXT.Core

-- | TMX format (translation memory) parser
--   Input is a .tmx file path.
parseTmx :: FilePath -> IO Corpus
parseTmx filePath = fmap mkCorpus . runX $
    readDocument [withValidate no] filePath >>> tmxParser

tmxParser :: ArrowXml a => a XmlTree [(Lang, String)]
tmxParser = getChildren >>>
    hasName "tmx" //> hasName "tu" >>> listA value
  where
    value = getChildren >>> hasName "tuv"
        &&& getAttrValue "xml:lang"
        >>> first (getChildren >>> hasName "seg" >>> getChildren >>> getText)
        >>^ swap


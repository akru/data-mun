module Data.OPUS.Tmx (parseTmx) where

import Data.OPUS.Types
import Data.OPUS.Loader

import Text.XML.HXT.Core
import Text.XML.HXT.Expat
import Data.Text (Text, pack)

-- | TMX format (translation memory) parser
--   Input is a .tmx file path.
parseTmx :: FilePath -> IO Corpus
parseTmx filePath = fmap mkCorpus . runX $
    readDocument [ withValidate no
                 , withExpat yes
                 ] filePath >>> tmxParser

tmxParser :: ArrowXml a => a XmlTree [(Lang, Text)]
tmxParser = getChildren >>>
    hasName "tmx" //> hasName "tu" >>> listA value
  where
    value = getChildren >>> hasName "tuv"
        &&& getAttrValue "xml:lang"
        >>> first (getChildren >>> hasName "seg" >>> getChildren >>> getText >>^ pack)
        >>^ swap


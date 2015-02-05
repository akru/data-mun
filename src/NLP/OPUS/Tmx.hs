module NLP.OPUS.Tmx (parseTmx) where

import NLP.OPUS.Types

import Text.XML.HXT.Core
import Text.XML.HXT.Expat
import Data.Text (Text, pack)
import Data.Map as M (fromList)
import Data.Set as S (fromList)

-- | TMX format (translation memory) parser
--   Input is a .tmx file path.
parseTmx :: FilePath -> IO Corpus
parseTmx filePath = fmap S.fromList . runX $
    readDocument [ withValidate no
                 , withExpat yes
                 ] filePath >>> tmxParser

tmxParser :: ArrowXml a => a XmlTree ParallelText
tmxParser = getChildren >>>
    hasName "tmx" //> hasName "tu" >>> listA value >>^ M.fromList
  where
    value = getChildren >>> hasName "tuv"
        &&& getAttrValue "xml:lang"
        >>> first (getChildren >>> hasName "seg" >>> getChildren >>> getText >>^ pack)
        >>^ swap


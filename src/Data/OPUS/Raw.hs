module Data.OPUS.Raw (parseRaw) where

import Data.OPUS.Types
import Data.OPUS.Loader

import Data.List (transpose)
import Text.XML.HXT.Core

-- | Raw OPUS format parser
--   Input is a list of pairs: language name - raw file path.
parseRaw :: [(Lang, FilePath)] -> IO Corpus
parseRaw langFiles =
    fmap (mkCorpus . consistentCheck . rawConverter) $
        mapM runParser files
  where
    files = map snd langFiles
    langs = map fst langFiles
    rawConverter = map (zip langs) . transpose
    runParser f  = runX $
        readDocument [withValidate no] f >>> rawParser
    consistentCheck c
        | all ((== length (head c)) . length) c = c
        | otherwise = error "unaligned source"


rawParser :: ArrowXml a => a XmlTree String
rawParser = getChildren >>>
    hasName "DOC" //> hasName "s" >>> getChildren >>> getText


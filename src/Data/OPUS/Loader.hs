module Data.OPUS.Loader
  ( load
  , save
  , unionParCorpus
  , mkParCorpus
  ) where

import Data.OPUS.Types

import qualified Data.ByteString.Lazy as LBS
import Codec.Compression.GZip
import Data.Serialize
import Data.Map hiding (map, foldl)

unionParCorpus :: ParCorpus -> ParCorpus -> ParCorpus
unionParCorpus = unionWith uni
  where
    a `uni` b = a `union` mapKeys (+ size a) b

mkParCorpus :: [[(Lang, String)]] -> ParCorpus
mkParCorpus = foldl unionParCorpus empty 
            . map (foldl unionParCorpus empty . map mkSimple)
  where
    mkSimple (l, s) = singleton l (singleton 0 s)

save :: Serialize a => a -> FilePath -> IO ()
save a name = LBS.writeFile name content
  where
    content = compress (encodeLazy a)
    
load :: Serialize a => FilePath -> IO (Either String a)
load name = do
    content <- LBS.readFile name
    return $ decodeLazy (decompress content)


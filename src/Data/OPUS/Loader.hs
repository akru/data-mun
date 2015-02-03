module Data.OPUS.Loader
  ( load
  , save
  , mkCorpus
  ) where

import Data.OPUS.Types

import qualified Data.ByteString.Lazy as LBS
import Data.Binary (Binary, encode, decode)
import Codec.Compression.GZip
import Data.Map as M hiding (map)
import Data.Set as S hiding (map)

mkCorpus :: [[(Lang, String)]] -> Corpus
mkCorpus = S.fromList . map M.fromList

save :: Binary a => FilePath -> a -> IO ()
save name =
    LBS.writeFile name . compress . encode
    
load :: Binary a => FilePath -> IO a
load name = do
    content <- LBS.readFile name
    return $ decode (decompress content)


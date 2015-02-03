module Data.OPUS.Loader where

import Data.OPUS.Types

import Codec.Compression.GZip (compress, decompress)
import qualified Data.ByteString.Lazy as LBS
import Data.Binary (Binary, encode, decode)
import Control.Applicative ((<$>))

-- | Save to compressed binary file
save :: Binary a => FilePath -> a -> IO ()
save name =
    LBS.writeFile name . compress . encode

-- | Load from compressed binary file
load :: Binary a => FilePath -> IO a
load name =
    decode . decompress <$> LBS.readFile name


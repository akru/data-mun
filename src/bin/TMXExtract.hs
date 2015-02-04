module Main where

import System.Environment
import Data.OPUS.Tmx
import Data.OPUS

main = do
    name <- fmap head getArgs
    c <- parseTmx name
    save "corpus.gz" c
    putStrLn "corpus.gz saved"

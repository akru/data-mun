module Main where

import System.Environment
import NLP.OPUS.Tmx
import NLP.OPUS

main :: IO ()
main = do
    name <- fmap head getArgs
    c <- parseTmx name
    save "corpus.gz" c
    putStrLn "corpus.gz saved"

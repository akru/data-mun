module Main where

import System.Environment
import NLP.OPUS

main :: IO ()
main = do
    (lang : caName : cbName : _) <- getArgs
    ca <- load caName
    cb <- load cbName
    save "corpus.gz" $ unionByLang lang ca cb
    putStrLn "corpus.gz saved"

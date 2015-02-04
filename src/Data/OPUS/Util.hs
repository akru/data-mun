module Data.OPUS.Util where

import Data.OPUS.Types

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Text (Text, unpack)
import System.Console.ANSI
import Data.Map ((!))

-- | Makes Corpus from list of paralles texts.
--   Parallel text presented as list of pairs: language name - text.
mkCorpus :: [[(Lang, Text)]] -> Corpus
mkCorpus = S.fromList . map M.fromList

-- | Union two corpuses by shared language
unionByLang :: Lang -> Corpus -> Corpus -> Corpus
unionByLang l a b = S.fromList corpusAB
  where
    corpusAB = [ M.union x y | x <- S.elems a
                             , y <- S.elems b
                             , x ! l == y ! l ]

-- | Parallel texts pretty printer
parallelView :: ParallelText -> IO ()
parallelView t = do
    setSGR [SetColor Foreground Vivid Magenta]
    putStr   (replicate 80 '-')
    mapM textView (coloredText t)
    setSGR [SetColor Foreground Vivid Magenta]
    putStrLn (replicate 80 '-')
    setSGR []
  where
    textView (intens, (lang, text)) = do
        setSGR [SetColor Foreground intens White]
        putStr ("\n" ++ lang ++ " :: ")
        putStrLn (unpack text)
    coloredText = zip (cycle [Dull, Vivid]) . M.toList

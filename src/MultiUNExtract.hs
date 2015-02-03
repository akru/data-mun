module Main where

import Data.OPUS.MultiUN
import Data.OPUS

import Data.List (groupBy, sort, sortBy)
import Control.Applicative ((<$>))
import Text.Read (readEither)
import System.FilePath.Posix
import Data.Either (rights)
import System.Environment
import System.Directory

extract :: FilePath -> [(Lang, FilePath)] -> IO ()
extract fileName input = do
    c <- loadXmlFiles input
    save (fileName ++ ".pc.gz") c

parallelYears :: [[String]] -> [String]
parallelYears =
    map show . parallelYears' [] . sortMap
  where
    sortMap :: [[String]] -> [[Int]]
    sortMap = map (sort . rights . map readEither)
    parallelYears' acc years
        | any null years = acc
        | otherwise =
            let heads = map head years
                m     = minimum heads 
            in if all (==m) heads
                then parallelYears' (m : acc) (map tail years)
                else parallelYears' acc (map (dropWhile (==m)) years)

dotsFilter :: [String] -> [String]
dotsFilter = filter ((/= '.') . head)

main :: IO ()
main = do
    args <- getArgs
    let source = args !! 0 </> "raw"
    langs <- dotsFilter <$> getDirectoryContents source
    years <- mapM (getDirectoryContents . (source </>)) langs

    let input      = [(l, y) | l <- langs, y <- parallelYears years]
        pathFix    = map (\(a, b) -> (a, source </> a </> b))
        nameByYear = map (\a -> (snd (head a), pathFix a))
                   . groupBy (\a b -> snd a == snd b)
                   . sortBy (\a b -> snd a `compare` snd b)
    mapM_ (uncurry extract) $ nameByYear input


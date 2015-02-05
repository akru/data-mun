module Main where

import NLP.OPUS.Raw
import NLP.OPUS

import Data.List (groupBy, sort, sortBy)
import Control.Applicative ((<$>))
import qualified Data.Set as Set
import Text.Regex.Posix ((=~))
import Text.Read (readEither)
import System.FilePath.Posix
import Data.Either (rights)
import System.Environment
import Control.Exception
import System.Directory

filePattern :: String
filePattern = "xml$"

extract :: FilePath -> [(Lang, FilePath)] -> IO ()
extract fileName input = do
    files <- map (sort . dotsFilter) <$> mapM getDirectoryContents dirs
    c <- extract' Set.empty files
    save (fileName ++ ".gz") c
  where
    dirs = map snd input
    extract' acc ([] : _) = return acc
    extract' acc ((x : xs) : ys)
        | x =~ filePattern && all (elem x) ys = do
            putStr $ "Loading " ++ x ++ "..."
            c <- try $ parseRaw $ map (fmap (\d -> d </> x)) input
            case c of
                Right corpus -> do
                    putStrLn $ show $ Set.size corpus
                    extract' (acc `Set.union` corpus) (xs : ys)
                Left e -> do
                    putStrLn $ show (e :: ErrorCall)
                    extract' acc (xs : ys)
                    
        | otherwise = do
            putStrLn $ "No parallel for " ++ x
            extract' acc (xs : ys)

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


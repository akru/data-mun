module Data.OPUS.MultiUN (loadXmlFiles) where

import Data.OPUS

import Data.ByteString.Lazy.UTF8 (ByteString(..), toString)
import qualified Data.ByteString.Lazy as LBS
import Data.Map as M hiding (map)
import Codec.Compression.GZip
import System.FilePath.Posix
import Text.HTML.TagSoup
import Text.Regex.Posix
import System.Directory
import Data.List (elem)

filePattern = "\\.xml\\.gz"

xmlToCorpus :: ByteString -> Corpus
xmlToCorpus =
    M.fromList . xmlToCorpus_ [] . parseTags . toString
  where
    xmlToCorpus_ acc tags = case tags of
        (TagOpen "s" [("id", i)] : TagText s : xs) -> xmlToCorpus_ ((read i, s) : acc) xs
        (_ : xs)                                   -> xmlToCorpus_ acc xs
        []                                         -> acc

loadXmlFiles :: [(Lang, FilePath)] -> IO ParCorpus
loadXmlFiles langDirs = do
    fileNames <- mapM getDirectoryContents dirNames
    loadFiles fileNames M.empty
  where
    dirNames = map snd langDirs
    langs    = map fst langDirs

    corpusSize = M.foldr ((+) . M.size) 0

    loadFiles :: [[String]] -> ParCorpus -> IO ParCorpus
    loadFiles files acc = case files of
        ([] : _)        -> return acc
        ((x : xs) : ys) -> do
            putStrLn $ "Coming " ++ show (length xs) ++ " files..."
            if x =~ filePattern && all (elem x) ys
                then do
                    putStr $ "Loading " ++ show x ++ "..."
                    mbCorpus <- loadXmls x
                    case mbCorpus of
                        Just corpus -> do
                            putStrLn $ show (corpusSize corpus)
                            loadFiles (xs : ys) (acc `unionParCorpus` corpus) 
                        Nothing -> do
                            putStrLn $ "broken!!!"
                            loadFiles (xs : ys) acc 
                else do
                    putStrLn $ "No parallel for " ++ show x
                    loadFiles (xs : ys) acc 

    loadXmls x = do
        let fileNames = map (\d -> d </> x) dirNames
        gzXmls <- mapM LBS.readFile fileNames
        let parCorpus  = map (xmlToCorpus . decompress) gzXmls
            parCorpSz  = map M.size parCorpus
            consistent = all (== head parCorpSz) parCorpSz 
        if consistent
            then return $ Just (M.fromList (zip langs parCorpus))
            else return $ Nothing


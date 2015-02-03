module Data.OPUS.MultiUN (loadXmlFiles) where

import Data.OPUS

import Data.ByteString.Lazy.UTF8 (ByteString(..), toString)
import qualified Data.ByteString.Lazy as LBS
import Data.IntMap as M hiding (map, null)
import Data.Set as S hiding (map, null) 
import Codec.Compression.GZip
import System.FilePath.Posix
import Text.HTML.TagSoup
import Text.Regex.Posix
import System.Directory
import Data.List (elem)

filePattern = "\\.xml\\.gz"

loadTexts :: ByteString -> [String]
loadTexts = loadTexts_ [] . parseTags . toString
  where
    loadTexts_ acc tags = case tags of
        (TagOpen "s" [("id", _)] : TagText s : xs) -> loadTexts_ (s : acc) xs
        (_ : xs)                                   -> loadTexts_ acc xs
        []                                         -> acc

loadXmlFiles :: [(Lang, FilePath)] -> IO Corpus
loadXmlFiles langDirs = do
    fileNames <- mapM getDirectoryContents dirNames
    loadFiles fileNames S.empty
  where
    dirNames = map snd langDirs
    langs    = map fst langDirs

    loadFiles :: [[String]] -> Corpus -> IO Corpus
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
                            putStrLn $ show (S.size corpus)
                            loadFiles (xs : ys) (acc `S.union` corpus) 
                        Nothing -> do
                            putStrLn $ "broken!!!"
                            loadFiles (xs : ys) acc 
                else do
                    putStrLn $ "No parallel for " ++ show x
                    loadFiles (xs : ys) acc 

    loadXmls x = do
        let fileNames = map (\d -> d </> x) dirNames
        gzXmls <- mapM LBS.readFile fileNames
        let parCorpus  = map (loadTexts . decompress) gzXmls
            parCorpSz  = map length parCorpus
            consistent = all (== head parCorpSz) parCorpSz 
        if consistent
            then return $ Just (mkCorpus (convert [] parCorpus))
            else return $ Nothing

    convert acc pc
        | any null pc = acc
        | otherwise = convert (zip langs (head pc) : acc) (tail pc)


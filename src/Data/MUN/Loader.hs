module Data.MUN.Loader
  ( loadXmlFiles, load, save
  , unionParCorpus
  ) where

import Data.MUN.Types

import Text.HTML.TagSoup
import Codec.Compression.GZip
import System.FilePath.Posix
import System.Directory
import Text.Regex.Posix
import Prelude as P

import Data.ByteString.Lazy.UTF8 (ByteString(..), toString)
import qualified Data.ByteString.Lazy as LBS
import Data.IntMap.Strict as IM hiding (map)
import Data.Map.Strict as M hiding (map)
import Data.List (elem)
import Data.Serialize

filePattern = "\\.xml\\.gz"

unionParCorpus :: ParCorpus -> ParCorpus -> ParCorpus
unionParCorpus = M.unionWith uni
  where
    a `uni` b = a `IM.union` IM.mapKeys (+ IM.size a) b

xmlToCorpus :: ByteString -> Corpus
xmlToCorpus =
    IM.fromList . xmlToCorpus_ [] . parseTags . toString
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

    corpusSize = M.foldr ((+) . IM.size) 0

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
            parCorpSz  = map IM.size parCorpus
            consistent = all (== head parCorpSz) parCorpSz 
        if consistent
            then return $ Just (M.fromList (zip langs parCorpus))
            else return $ Nothing

save :: Serialize a => a -> FilePath -> IO ()
save a name = LBS.writeFile name content
  where
    content = compress (encodeLazy a)
    
load :: Serialize a => FilePath -> IO (Either String a)
load name = do
    content <- LBS.readFile name
    return $ decodeLazy (decompress content)


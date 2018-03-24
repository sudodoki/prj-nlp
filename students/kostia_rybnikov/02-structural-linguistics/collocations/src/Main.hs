{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Data.Default
import Data.Foldable (foldl')
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Data.Semigroup
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import NLP.CoreNLP
import System.Environment

type Verb = Text

type Adverb = Text

type VerbTokenId = Int

type DependentTokenId = Int

filterDataLines :: [Verb] -> [Text] -> [Text]
filterDataLines verbs = filter hasVerb
  where
    hasVerb line = any (`T.isInfixOf` line) verbs

getTopAdverbs :: [Verb] -> [ParsedDocument] -> HashMap Verb (HashMap Adverb Int)
getTopAdverbs verbs docs = combine (map getDocTopAdverbs docs)
  where
    combine = foldl' (H.unionWith (H.unionWith (+))) H.empty
    getDocTopAdverbs ParsedDocument {..} =
      sumAdverbs (dependentsAdverbs (verbsDependentIds verbsWithIndexes))
      where
        verbsWithIndexes = map verbWithIndexes verbs
        verbWithIndexes verb = (verb, verbIndexes verb)
        verbIndexes verb = concatMap (verbIndexesSent verb) (sentences doc)
        verbIndexesSent verb Sentence {..} =
          catMaybes (map (verbIndexesTok verb) tokens)
        verbIndexesTok verb Token {..}
          | pos `elem` [VB, VBD, VBG, VBN, VBP, VBZ] && lemma == verb =
            Just index
          | otherwise = Nothing
        verbsDependentIds = map verbDependentIds
        -- | find all dependentIds where governor=verbTokenId
        verbDependentIds (verb, verbTokIds) =
          (verb, concatMap verbDependentIdsForVerbTokId verbTokIds)
        verbDependentIdsForVerbTokId verbTokId =
          concatMap (verbDependentIdsForVerbTokIdSent verbTokId) (sentences doc)
        verbDependentIdsForVerbTokIdSent verbTokId Sentence {..} =
          catMaybes
            (map
               (verbDependentIdsForVerbTokIdToken verbTokId)
               enhancedDependencies)
        verbDependentIdsForVerbTokIdToken verbTokId Dependency {..}
          | "ly" `T.isInfixOf` dependentGloss && governor == verbTokId =
            Just dependent
          | otherwise = Nothing
        dependentsAdverbs xs = map dependentAdverbs xs
        dependentAdverbs (verb, depTokIds) = (verb, getAdverbsByIds depTokIds)
        getAdverbsByIds tokIds = concatMap getAdverbsById tokIds
        getAdverbsById tokId =
          concatMap (getAdverbsByIdSent tokId) (sentences doc)
        getAdverbsByIdSent tokId Sentence {..} =
          catMaybes (map (getAdverbsByIdSentTok tokId) tokens)
        getAdverbsByIdSentTok tokId Token {..}
          | index == tokId && pos `elem` [RB, RBR, RBS, WRB] = Just word
          | otherwise = Nothing
        sumAdverbs xs = H.fromListWith mergeMaps (map toHm xs)
          where
            toHm (v, advs) = (v, H.fromListWith (+) (map (\x -> (x, 1)) advs))
            mergeMaps = H.unionWith (+)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [corenlpPath, cacheDb, blogdataPath] -> do
      blogDataLines <- T.lines <$> T.readFile blogdataPath
      let corenlpOpts =
            def {cacheDb = Just cacheDb, numWorkers = 1, memSizeMb = 5000} :: LaunchOptions
      let verbs =
            [ "say"
            , "tell"
            , "speak"
            , "claim"
            , "communicate"
            , "yell"
            , "describe"
            , "express"
            , "advice"
            , "announce"
            , "notify"
            ]
      let filtered = filterDataLines verbs blogDataLines
      -- let filtered =
      --       [ "NBC News has officially announced that they 've picked White House correspondent David Gregory to be the new host of Meet The Press ."
      --       ]
      putStrLn $ "Going to process lines: " ++ show (length filtered)
      parsedDocs <- launchCoreNLP corenlpPath corenlpOpts filtered
      let res = getTopAdverbs verbs parsedDocs
      forM_ (H.toList res) $ \(verb, counts) -> do
        T.putStrLn $
          verb <> ": " <>
          T.pack
            (show (take 10 (sortBy (flip compare `on` snd) (H.toList counts))))
    _ ->
      putStrLn
        "Usage: stack exec collocations -- /path/to/standford-corenlp /path/to/cache/db /path/to/blog2008.txt"

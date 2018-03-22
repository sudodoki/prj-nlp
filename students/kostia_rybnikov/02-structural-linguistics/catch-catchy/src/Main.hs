{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
import Data.Decimal
import Data.Default
import qualified Data.HashMap.Strict as H
import Data.Maybe (catMaybes)
import Data.Semigroup
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import NLP.CoreNLP
import NLP.CoreNLP (LaunchOptions(..))
import qualified NLP.SentiwordnetParser as Senti
import NLP.SentiwordnetParser
import System.Environment
import Text.Trifecta (Result(..))

pennPosToSentiPos :: PennPOS -> Maybe Senti.POS
pennPosToSentiPos cp
  | cp `elem` [NN, NNS, NNP, NNPS, PRP] = Just Senti.Noun
pennPosToSentiPos cp
  | cp `elem` [VB, VBD, VBG, VBN, VBP, VBZ] = Just Senti.Verb
pennPosToSentiPos cp
  | cp `elem` [JJ, JJR, JJS] = Just Senti.Adjective
pennPosToSentiPos cp
  | cp `elem` [RB, RBR, RBS] = Just Senti.Adverb
pennPosToSentiPos _
  | otherwise = Nothing

-- | We take "(pos+neg)/2" as a score for a single meaning
lookupScore :: SentiWordNetLookup -> Senti.POS -> Text -> Maybe Decimal
lookupScore sentiTable sentiPos w =
  fmap getScore (H.lookup (sentiPos, w) sentiTable)
  where
    getScore items =
      let topItems = filter isTopPosition items
          scores = map getOneScore topItems
      in (sum scores) / fromIntegral (length scores)
    isTopPosition SentiWordNetLookupItem {..} = lookPos <= 5
    getOneScore SentiWordNetLookupItem {..} = (lookPosScore + lookNegScore) / 2

getSentimentHeadlines ::
     SentiWordNetLookup -> [ParsedDocument] -> [ParsedDocument]
getSentimentHeadlines sentiTable = filter isSentiment
  where
    isSentiment ParsedDocument {..} = isSentimentDoc doc
    isSentimentDoc Document {..} = any isSentimentSentence sentences
    isSentimentSentence Sentence {..} = any isSentimentToken tokens
    isSentimentToken Token {..} =
      case pennPosToSentiPos pos of
        Nothing -> False
        Just sentiPos ->
          let lemmaRes = lookupScore sentiTable sentiPos lemma
              wordRes = lookupScore sentiTable sentiPos word
          in case (lemmaRes <|> wordRes) of
               Nothing -> False
               Just score -> score >= 0.5

getSuperlativesOrProminence :: [ParsedDocument] -> [Text]
getSuperlativesOrProminence = catMaybes . map toTitle
  where
    toTitle x = toSuperlative x <|> toProminence x
    toSuperlative ParsedDocument {..} =
      if isSuperlative doc
        then Just origText
        else Nothing
    toProminence ParsedDocument {..} =
      if isProminent doc
        then Just origText
        else Nothing
    isSuperlative Document {..} = any isSuperlativeInSentence sentences
    isSuperlativeInSentence Sentence {..} = any isSuperlativeToken tokens
    isSuperlativeToken Token {..} = pos `elem` [JJS, RBS]
    isProminent Document {..} = any isProminentInSentence sentences
    isProminentInSentence Sentence {..} = any isProminentToken tokens
    isProminentToken Token {..} =
      ner `elem`
      [ PERSON
      , LOCATION
      , ORGANIZATION
      , MISC
      , CITY
      , STATE_OR_PROVINCE
      , COUNTRY
      , NATIONALITY
      , TITLE
      , IDEOLOGY
      , CRIMINAL_CHARGE
      , CAUSE_OF_DEATH
      ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [corenlpPath, sentiwordnetPath, examinerHeadlinesPath, cacheDb, outputFile] -> do
      headlines <- T.lines <$> T.readFile examinerHeadlinesPath
      sentiData <- T.readFile sentiwordnetPath
      let corenlpOpts = def {cacheDb = Just cacheDb} :: LaunchOptions
      parsedDocs <- launchCoreNLP corenlpPath corenlpOpts headlines
      case Senti.parse sentiData of
        Failure e -> error ("Failed to parse SentiWordNet data: " ++ show e)
        Success senti -> do
          let sentiTable = toSentiWordNetLookup senti
          let sentimentHeadlines =
                map origText (getSentimentHeadlines sentiTable parsedDocs)
          let coreNlpTitles = getSuperlativesOrProminence parsedDocs
          let unique =
                Set.toList
                  (Set.fromList sentimentHeadlines <> Set.fromList coreNlpTitles)
          T.writeFile outputFile (T.unlines unique)
          return ()
    _ ->
      putStrLn
        "Usage: stack exec catch-catchy -- /path/to/standford-corenlp /path/to/sentiwordnet.txt /path/to/examiner-headlines.txt /path/to/cache/db ./output.txt"

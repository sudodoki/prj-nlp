{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative
import Control.Lens hiding (index, re)
import Control.Monad
import Data.Aeson.Lens
import Data.Default
import qualified Data.List as List
import Data.Maybe
import Data.Semigroup
import qualified Data.String.Class as S
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Tree
import Formatting
import Formatting.ShortFormatters (sh, st)
import NLP.CoreNLP
import qualified Network.Wreq as W
import Safe
import System.Environment
import Text.HTML.Scalpel.Core
import qualified Text.HTML.Scalpel.Core as SC
import Text.Pandoc hiding (trace)
import Text.Regex.PCRE.Heavy

data Book = Book
  { title :: Text
  , mpubYear :: Maybe Int
  } deriving (Show, Eq)

parseBooks :: Scraper Text [Book]
parseBooks = do
  chroots ("tr" @: ["itemtype" @= "http://schema.org/Book"]) $ do
    title <- chroot ("a" @: [hasClass "bookTitle"]) (SC.text "span")
    pubYearRaw <- SC.text $ "td" @: ["width" @= "100%"]
    let mRes = scan [re|published[^0-9]*([0-9]+)|] pubYearRaw
    let mpubYear = mRes & headMay <&> snd >>= headMay <&> T.unpack >>= readMay
    return Book {..}

getWikiBody :: Text -> Maybe Text
getWikiBody r =
  r ^? key "query" . key "pages" . key "2400008" . key "revisions" . nth 0 .
  key "*" .
  _String

downloadGoodreads :: IO Text
downloadGoodreads = do
  res <- W.get "https://www.goodreads.com/author/list/16593.Sam_Harris"
  return (res ^. W.responseBody & S.toText)

downloadWikiBody :: IO Text
downloadWikiBody = do
  r <-
    W.get
      "https://en.wikipedia.org/w/api.php?action=query&titles=Sam_Harris&prop=revisions&rvprop=content&format=json"
  return $ fromMaybe "" $ getWikiBody (r ^. W.responseBody . to S.toText)

sentToText :: Sentence -> Text
sentToText sent = T.intercalate " " (map originalText (tokens sent))

-- | Title claimed in the text
type ClaimedTitle = Text

type ClaimedYear = Int

data Claim
  -- | things like `The End of Faith (2004)`
  = BookParenthsYear ClaimedYear
                     Book
  -- | things like `He published a long-form essay _ Lying _ in 2011`
  | HePublishedIn ClaimedYear
                  Book
  deriving (Show, Eq)

data SentClaim = SentClaim
  { scSentence :: Sentence
  , scClaims :: [Claim]
  } deriving (Show, Eq)

factCheck :: [ParsedDocument] -> [Book] -> IO ()
factCheck docs books = mapM_ factCheckDoc docs
  where
    factCheckDoc :: ParsedDocument -> IO ()
    factCheckDoc ParsedDocument {..} = do
      let sentClaims = map (extractFacts books) (sentences doc)
      forM_ (catMaybes sentClaims) $ \SentClaim {..} -> do
        forM_ (List.nub scClaims) $ \case
          BookParenthsYear y b@Book {..} -> do
            putStrLn "[BookParenthsYear]"
            case mpubYear of
              Just pubYear
                | pubYear == y -> T.putStrLn (correctClaim b scSentence)
                | otherwise -> T.putStrLn (incorrectClaim b scSentence)
              Nothing -> T.putStrLn (uncheckableClaim b scSentence)
            putStrLn ""
          HePublishedIn y b@Book {..} -> do
            putStrLn "[HePublishedIn]"
            case mpubYear of
              Just pubYear
                | pubYear == y -> T.putStrLn (correctClaim b scSentence)
                | otherwise -> T.putStrLn (incorrectClaim b scSentence)
              Nothing -> T.putStrLn (uncheckableClaim b scSentence)
            putStrLn ""
    correctClaim b s =
      sformat
        ("Correct claim! Book: " %sh % "\nSentence: \n" %st)
        b
        (sentToText s)
    incorrectClaim b s =
      sformat
        ("A mis-claim! Book: " %sh % "\nSentence: \n" %st)
        b
        (sentToText s)
    uncheckableClaim b s =
      sformat
        ("Not enough data to check the claim! Book: " %sh % "\nSentence: \n" %st)
        b
        (sentToText s)

extractFacts :: [Book] -> Sentence -> Maybe SentClaim
extractFacts books s@Sentence {..} =
  case claims of
    [] -> Nothing
    _ -> Just (SentClaim s claims)
  where
    deps = enhancedPlusPlusDependencies
    claims :: [Claim]
    claims = concat [bookParenthsYears, hePublishedIn]
    -- | This rule works rather with a list of words in a sentence
    bookParenthsYears = do
      b <- books
      bookTitle <- possibleTitles b
      let titleWords = splitTitle bookTitle
      toks <- possibleTokens
      titlePos <- subListPos titleWords toks
      let yearPos = titlePos + length titleWords
      year <- yearAt yearPos <|> yearAt (yearPos + 1) <|> yearAt (yearPos + 2)
      return (BookParenthsYear year b)
    -- | This rule works with a dependency tree. While more complex
    -- constructs like recursively following the comma are possible,
    -- it is quite hard to develop that as a homework, so I'm leaving
    -- this exercise for future. Multi-word titles are also possible
    -- to be dealt with, but would probably need to extend the data
    -- structures to leave a connection between parsed dependencies
    -- and original sentence, so that it's easy to compare them in a
    -- strict order.
    hePublishedIn = do
      publ <- deps
      guard (T.toLower (dependentGloss publ) == "published")
      heDep <- depChildren publ
      guard (T.toLower (dependentGloss heDep) == "he")
      dobj <- depChildren publ
      guard (dep dobj == "dobj")
      book <- depChildren dobj
      bookCandidate <- books
      bookCandidateTitle <- possibleTitles bookCandidate
      guard (bookCandidateTitle == dependentGloss book)
      in_ <- deps
      guard (governor in_ == dependent book)
      guard (dep in_ == "nmod:in")
      yearInt <- maybeToList (readMay (T.unpack (dependentGloss in_)))
      return (HePublishedIn yearInt bookCandidate)
    depChildren x = filter (\child -> governor child == dependent x) deps
    -- | When the book is called "Waking Up: A Guide to Spirituality
    -- Without Religion" we use "Waking Up" as well
    possibleTitles :: Book -> [Text]
    possibleTitles Book {..} =
      title : T.filter (\x -> not (x `elem` (":.!?:" :: String))) title :
      maybeToList (headMay (T.splitOn ":" title))
    possibleTokens =
      let ts = map originalText tokens
      in [ts, filter (\x -> not (x `elem` [":"])) ts]
    yearAt :: Int -> [ClaimedYear]
    yearAt i = do
      w <- maybeToList (fmap originalText (tokens `atMay` i))
      maybeToList (readMay (T.unpack w))
    splitTitle = T.words

-- | Finds all sublist positions in a list
--
-- TODO: make this specialized to text and a bit more fuzzy in future
subListPos :: Eq a => [a] -> [a] -> [Int]
subListPos needle xs =
  mapMaybe
    (\(i, eq) ->
       if eq
         then Just i
         else Nothing)
    (zip [0 .. length xs] (map (needle `List.isPrefixOf`) (iterate tailSafe xs)))

sentToTree :: Sentence -> Tree String
sentToTree Sentence {..} =
  let [root] =
        filter (\Dependency {..} -> governor == 0) enhancedPlusPlusDependencies
  in go enhancedPlusPlusDependencies 0 root
  where
    go :: [Dependency] -> Int -> Dependency -> Tree String
    go deps currDepth dep' =
      let childDeps =
            if currDepth >= 9
              then []
              else filter (\x -> dependent dep' == governor x) deps
          subForest = map (go deps (currDepth + 1)) childDeps
      in Node
           (T.unpack
              (dep dep' <> " - " <> dependentGloss dep' <> " (gov=" <>
               governorGloss dep' <>
               ")"))
           subForest

showDocTree :: ParsedDocument -> IO ()
showDocTree ParsedDocument {..} = do
  forM_ (sentences doc) $ \sent -> do
    let tree = sentToTree sent
    let s = drawTree tree
    T.putStrLn $ sentToText sent
    putStrLn s

main :: IO ()
main = do
  args <- getArgs
  case args of
    [corenlp, cache] -> do
      t <- downloadGoodreads
      -- t <- T.readFile "data/goodreads_samharris.html"
      let books = scrapeStringLike t parseBooks
      mwikiBody <- Just <$> downloadWikiBody
      -- mwikiBody <- getWikiBody <$> T.readFile "data/wiki_api_response.json"
      let wikiBody = fromMaybe (error "failed to get wiki body") mwikiBody
      let wikiTxt =
            (either
               (error . show)
               Prelude.id
               (runPure
                  (do mr <- readMediaWiki def wikiBody
                      writePlain def {writerWrapText = WrapNone} mr)))
      let wikiTxtLines = T.lines wikiTxt
      docs <-
        launchCoreNLP
          corenlp
          def {numWorkers = 3, cacheDb = Just cache, chunkSize = 50}
          wikiTxtLines
      factCheck docs (fromMaybe [] books)
      return ()
    _ ->
      putStrLn "Usage: stack exec factcheck -- /path/to/corenlp /path/to/cache"

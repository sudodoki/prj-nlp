{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad.Trans.Resource
import Data.Conduit (ConduitT, (.|), runConduit)
import Data.Default (def)
import Data.Foldable
import Data.Hashable
import Data.List (sortBy)
import Data.Maybe
import Data.String (IsString(fromString))
import qualified Data.Text as T
import Data.Text (Text)
import Data.XML.Types (Event)
import GHC.Generics (Generic)
import System.Environment
import Text.Groom
import Text.Pandoc
import Text.XML.Stream.Parse

type Synonym = Text

data Page = Page
  { title :: Text
  , synonyms :: [[Synonym]]
  } deriving (Show, Eq, Generic, Hashable)

data MediaWiki = MediaWiki
  { pages :: [Page]
  } deriving (Show, Eq, Generic, Hashable)

-- | prefix with a namespace
p :: IsString a => String -> a
p x = fromString ("{http://www.mediawiki.org/xml/export-0.10/}" ++ x)

-- | Extract the part between the "{{-sin-}}" block and whatever comes
-- after (if any). We want this because Pandoc parser sometimes fails
-- on complex markup with tags and I have no will to improve it right
-- now
--
-- > extractSynPart "foo\n{{-sin-}}\n*[[azzurrognolo]]\n\n{{-alter-}}bar"
-- "\n*[[azzurrognolo]]\n\n"
--
extractSynParts :: Text -> [Text]
extractSynParts t =
  case T.splitOn needle t of
    (_:xs) -> map getBeforeNextTag xs
    _ -> []
  where
    needle = "{{-sin-}}"
    getBeforeNextTag x =
      case T.splitOn "{{-" x of
        (y:_) -> y
        _ -> ""

extractSynonyms :: Text -> [[Synonym]]
extractSynonyms text =
  let synParts = extractSynParts text
      parsedParts :: [Pandoc]
      parsedParts = mapMaybe (skipErr . runPure . readMediaWiki def) synParts
      procPandoc :: Pandoc -> [[Synonym]]
      procPandoc (Pandoc _meta blocks) = concatMap procBlock blocks
      procBlock :: Block -> [[Synonym]]
      procBlock (BulletList xs) = map procLine xs
      procBlock _ = []
      procLine :: [Block] -> [Synonym]
      procLine [Plain blocks] = mapMaybe procLineInline blocks
      procLine _ = []
      procLineInline :: Inline -> Maybe Synonym
      procLineInline (Link _ [Str x] _) = Just (T.pack x)
      procLineInline _ = Nothing
  in filter (/= []) (concatMap procPandoc parsedParts)
  where
    skipErr (Left _pe) = Nothing
    skipErr (Right pd) = Just pd

parsePage :: MonadThrow m => ConduitT Event o m (Maybe Page)
parsePage = do
  tagNoAttr (p "page") $ do
    title <- fromMaybe "" <$> tagNoAttr (p "title") content
    _ <- ignoreTreeContent (p "ns")
    _ <- ignoreTreeContent (p "id")
    _ <- ignoreTreeContent (p "redirect")
    _ <- ignoreTreeContent (p "restrictions")
    text <-
      fmap (fromMaybe "" . fromMaybe Nothing) $
      tagNoAttr (p "revision") $ do
        _ <- ignoreTreeContent (p "id")
        _ <- ignoreTreeContent (p "parentid")
        _ <- ignoreTreeContent (p "timestamp")
        _ <- ignoreTreeContent (p "contributor")
        _ <- ignoreTreeContent (p "minor")
        _ <- ignoreTreeContent (p "comment")
        _ <- ignoreTreeContent (p "model")
        _ <- ignoreTreeContent (p "format")
        t <- tag' (p "text") ignoreAttrs $ const content
        _ <- ignoreTreeContent (p "sha1")
        return t
    let synonyms = extractSynonyms text
    return Page {..}

parseMediaWiki :: MonadThrow m => ConduitT Event o m (Maybe MediaWiki)
parseMediaWiki = do
  tag' (p "mediawiki") ignoreAttrs $ \_ -> do
    _ <- ignoreTreeContent (p "siteinfo")
    pages <- many parsePage
    return (MediaWiki pages)

hasSynonyms :: Page -> Bool
hasSynonyms Page {..} =
  case synonyms of
    (_:_) -> True
    _ -> False

moreSynonyms :: Page -> Page -> Ordering
moreSynonyms page1 page2 =
  compare (pageSyns (synonyms page1)) (pageSyns (synonyms page2))
  where
    pageSyns :: [[Synonym]] -> Int
    pageSyns xs = foldl' (+) 0 (map length xs)

main :: IO ()
main
  -- let fpath =
  --       "/home/kb/Downloads/wiktionary/it/20180301/itwiktionary-20180301-pages-articles.xml/data"
 = do
  [fpath] <- getArgs
  mediaWiki <-
    runResourceT $
    runConduit $
    parseFile def fpath .| force "mediawiki required" parseMediaWiki
  let elementsWithSynonyms = filter hasSynonyms (pages mediaWiki)
  putStrLn $
    "> Found elements with synonyms: " ++ show (length elementsWithSynonyms)
  putStrLn $ "> Top 5 pages with most synonyms:"
  putStrLn $ groom $ take 5 (sortBy (flip moreSynonyms) elementsWithSynonyms)

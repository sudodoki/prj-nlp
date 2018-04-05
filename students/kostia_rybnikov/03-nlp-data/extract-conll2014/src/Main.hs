{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

--import Text.Groom
import Control.Monad.Trans.Resource
import Data.Conduit (ConduitT, (.|), runConduit)
import Data.Foldable
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HS
import Data.Hashable
import Data.Maybe
import Data.Semigroup
import qualified Data.String.Class as S
import Data.Text (Text)
import Data.XML.Types (Event)
import GHC.Generics (Generic)
import Safe
import System.Environment
import Text.XML
import Text.XML.Stream.Parse

data Mistake = Mistake
  { startPar :: Int
  , startOff :: Int
  , endPar :: Int
  , endOff :: Int
  , mistakeType :: Text
  , correction :: Maybe Text
  } deriving (Show, Eq, Generic, Hashable)

type TeacherId = Int

data Annotation = Annotation
  { teacherId :: TeacherId
  , mistakes :: [Mistake]
  } deriving (Show, Eq)

type Nid = Text

data Doc = Doc
  { nid :: Nid
  , annotations :: [Annotation]
  } deriving (Show, Eq)

-- | We compare mistakes per documents
type PerTeacherMistakes = HashMap TeacherId (HS.HashSet (Nid, Mistake))

parseMistake :: MonadThrow m => ConduitT Event o m (Maybe Mistake)
parseMistake =
  tag'
    "MISTAKE"
    ((,,,) <$> requireAttr "start_par" <*> requireAttr "start_off" <*>
     requireAttr "end_par" <*>
     requireAttr "end_off") $ \(sp, so, ep, eo) -> do
    startPar <- pread sp
    startOff <- pread so
    endPar <- pread ep
    endOff <- pread eo
    mistakeType <- fromMaybe "" <$> tagNoAttr "TYPE" content
    correction <- fromMaybe Nothing <$> tagNoAttr "CORRECTION" contentMaybe
    _comment <- ignoreTreeContent "COMMENT"
    return (Mistake {..})

pread :: (Read a, S.ConvString s, Monad m) => s -> m a
pread x =
  maybe
    (fail ("Couldn't read: " ++ (S.toString x)))
    return
    (readMay (S.toString x))

parseAnnotation :: MonadThrow m => ConduitT Event o m (Maybe Annotation)
parseAnnotation =
  tag' "ANNOTATION" (requireAttr "teacher_id") $ \ti -> do
    teacherId <- pread ti
    mistakes <- many parseMistake
    return $ Annotation {..}

parseDoc :: MonadThrow m => ConduitT Event o m (Maybe Doc)
parseDoc =
  tag' "DOC" (requireAttr "nid") $ \nid -> do
    _ <- ignoreTreeContent "TEXT"
    anns <- many parseAnnotation
    return (Doc nid anns)

-- parseDocs :: MonadThrow m => ConduitT Event o m (Maybe [Doc])
-- parseDocs = tagNoAttr "DOCUMENTS" $ many parseDoc

perTeacherMistakes :: [Doc] -> PerTeacherMistakes
perTeacherMistakes docs = H.fromListWith (<>) (concatMap fromDoc docs)
  where
    fromDoc Doc {..} = concatMap (fromAnnotation nid) annotations
    fromAnnotation nid Annotation {..} =
      map (fromMistake teacherId nid) mistakes
    fromMistake ::
         TeacherId -> Nid -> Mistake -> (TeacherId, HS.HashSet (Nid, Mistake))
    fromMistake teacherId nid mistake = (teacherId, HS.singleton (nid, mistake))
  --   fromAnnotation Doc{..} Annotation {..} = (teacherId, HS.fromList mistakes)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fpath] -> do
      docs <-
        runResourceT $
        runConduit $ parseFile def fpath .| force "documents required" (fmap Just (many parseDoc))
      print (length docs)
      let mistakes = perTeacherMistakes docs
      -- putStrLn $ groom mistakes
      let keys = H.keys mistakes
      putStrLn $ "Found teachers: " <> show (length keys)
      putStrLn $
        "Found overall mistakes: " <>
        show (length (concatMap HS.toList mistakes))
      let mistakesIntersection = fold (H.elems mistakes)
      putStrLn $
        "Mistake annotations which intersect upon all teachers: " ++
        show (length mistakesIntersection)
    _ ->
      putStrLn
        "Usage: stack exec -- extract-conll2014 /path/to/official-2014.1.sgml"

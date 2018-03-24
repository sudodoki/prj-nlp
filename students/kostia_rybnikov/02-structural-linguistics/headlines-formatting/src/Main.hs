{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main
  ( main
  , reconstructPositioned
  ) where

import Control.Monad (forM)
import Data.Foldable
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Data.Vector ((!), (//))
import NLP.Corpora.Conll (Tag)
import qualified NLP.Corpora.Conll as C
import qualified NLP.POS as POS
import NLP.Types (POSTagger)
import qualified NLP.Types.Tree as POS
import NLP.Types.Tree (POS(..), TaggedSentence(..), showTok)

-- | For the reconstruction and analysis porpuses, we'll need to keep
-- the knowledge of wether the tag is first or last in the sentence
data POSTagMarked = POSTagMarked
  { markedOrig :: POS Tag
  , markedIsFirst :: Bool
  , markedIsLast :: Bool
  } deriving (Show, Eq)

-- | Show a marked POS token
showptm :: POSTagMarked -> Text
showptm = showTok . posToken . markedOrig

-- | Chatter doesn't give tag's position, but we want that for the
-- reconstruction. Because of a lack of time I decided not to extend
-- the 'chatter' library but rather go through parsed POSes and match
-- them against positions in the original text
data PositionedPOS = PositionedPOS
  { pos :: POSTagMarked
  , position :: Int
  } deriving (Show, Eq)

positionPOSes :: POSTagger Tag -> Text -> [PositionedPOS]
positionPOSes tagger title = go 0 title tags
  where
    go :: Int -> Text -> [POSTagMarked] -> [PositionedPOS]
    go _ "" [] = []
    go _ "" (_:_) = error "Some tags couldn't be positioned"
    go position t (x:xs) =
      let (dropped, t2) = T.breakOn (showptm x) t
          t3 = T.drop (T.length (showptm x)) t2
          posPosition = position + T.length dropped
          traversePosition = posPosition + T.length (showptm x)
      in (PositionedPOS x posPosition) : go traversePosition t3 xs
    go _ _ [] = []
    sentences :: [TaggedSentence Tag]
    sentences = POS.tag tagger title
    tags :: [POSTagMarked]
    tags = concatMap (sentenceToMarked . POS.unTS) sentences
    sentenceToMarked :: [POS Tag] -> [POSTagMarked]
    sentenceToMarked [] = []
    sentenceToMarked xss =
      let marked = V.fromList (map mkMarked xss)
      in V.toList ((setFirst . setLast) marked)
    setFirst marked = marked // [(0, (marked ! 0) {markedIsFirst = True})]
    setLast marked =
      let mLen = V.length marked
      in marked // [(mLen - 1, (marked ! (mLen - 1)) {markedIsLast = True})]
    mkMarked x = POSTagMarked x False False

-- | Not required but nice to have
reconstructPositioned :: [PositionedPOS] -> Text
reconstructPositioned positionedPoses = go 0 positionedPoses
  where
    go :: Int -> [PositionedPOS] -> Text
    go _ [] = ""
    go currLength (x:xs) =
      let i = position x
      in T.replicate (i - currLength) " " <> showptm (pos x) <>
         go (i + T.length (showptm (pos x))) xs

-- | Let's keep the spacing as is
data POSOrSpacing
  = ThePOS POSTagMarked
  | TheSpacing Text
  deriving (Show, Eq)

positionedToPOSOrSpacing :: [PositionedPOS] -> [POSOrSpacing]
positionedToPOSOrSpacing pss = go 0 pss
  where
    go :: Int -> [PositionedPOS] -> [POSOrSpacing]
    go _ [] = []
    go i (x:xs) =
      let thePOS = ThePOS (pos x)
          spacingLen = position x - i
          rest = thePOS : go (i + T.length (showptm (pos x)) + spacingLen) xs
      in if position x > i
           then let theSpacing = TheSpacing (T.replicate spacingLen " ")
                in theSpacing : rest
           else rest

renderPOSOrSpacing :: POSOrSpacing -> Text
renderPOSOrSpacing (ThePOS p) = showptm p
renderPOSOrSpacing (TheSpacing spc) = spc

reconstructPOSOrSpacing :: [POSOrSpacing] -> Text
reconstructPOSOrSpacing = foldl' (<>) "" . map renderPOSOrSpacing

-- | The most interesting function here
modifyByRules :: [POSOrSpacing] -> [POSOrSpacing]
modifyByRules [] = []
modifyByRules (x@(TheSpacing _):xs) = x : modifyByRules xs
modifyByRules (x@(ThePOS pos):xs)
  | (posTag (markedOrig pos) `elem` capitalizeTags) ||
      markedIsFirst pos || markedIsLast pos =
    ThePOS (capitalizePOS pos) : modifyByRules xs
  | (posTag (markedOrig pos) `elem` lowerTags) =
    ThePOS (lowercasePOS pos) : modifyByRules xs
  | otherwise = x : modifyByRules xs
  where
    capitalizePOS :: POSTagMarked -> POSTagMarked
    capitalizePOS posm@POSTagMarked {..} =
      pos
      { markedOrig =
          markedOrig {posToken = POS.Token (capitalize (showptm posm))}
      }
    capitalize = T.intercalate "-" . map capitalizeSingle . T.splitOn "-"
    capitalizeSingle (T.uncons -> Nothing) = ""
    capitalizeSingle (T.uncons -> Just (y, ys)) =
      T.toUpper (T.singleton y) <> ys
    capitalizeSingle _ = error "impossible!"
    lowercasePOS :: POSTagMarked -> POSTagMarked
    lowercasePOS posm@POSTagMarked {..} =
      posm
      { markedOrig =
          markedOrig {posToken = POS.Token (T.toLower (showptm posm))}
      }
    lowerTags = [C.CC, C.RP, C.UH]
    capitalizeTags =
      [ C.NN
      , C.NNS
      , C.NNP
      , C.NNPS
      , C.PRP
      , C.PRPdollar
      , C.JJ
      , C.JJR
      , C.JJS
      , C.VB
      , C.VBD
      , C.VBG
      , C.VBN
      , C.VBP
      , C.VBZ
      , C.RB
      , C.RBR
      , C.RBS
      , C.WRB
      , C.IN
      ]

main :: IO ()
main = do
  f <-
    T.readFile
      "../../../../tasks/02-structural-linguistics/examiner-headlines.txt"
  tagger <- POS.defaultTagger
  let titles = T.lines f
  res <-
    forM titles $ \title -> do
      let spaced = positionedToPOSOrSpacing (positionPOSes tagger title)
          modified = modifyByRules spaced
          modifiedRendered = reconstructPOSOrSpacing modified
      return (modifiedRendered == title, modifiedRendered)
  let correctlyStated = length (filter (== True) (map fst res))
  putStrLn ("Number of titles stated correctly: " <> show correctlyStated)
  let newTitles = map snd res
  T.writeFile "examiner-headlines-corrected.txt" (T.unlines newTitles)
  putStrLn "Corrected headlines were written into examiner-headlines-corrected.txt"
  return ()

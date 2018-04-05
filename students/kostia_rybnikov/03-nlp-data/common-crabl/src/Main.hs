{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Main where

import Control.Arrow (second)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.HashMap.Monoidal (MonoidalHashMap)
import qualified Data.HashMap.Monoidal as MH
import Data.IORef
import qualified Data.List as List
import Data.List (sortBy)
import Data.Maybe
import Data.Semigroup
import qualified Data.String.Class as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Warc
import Network.URI
import qualified Pipes as P
import Pipes.ByteString (fromHandle)
import qualified Pipes.GZip as PGZip
import System.Environment
import System.IO
import Text.CLD2
import Text.Groom

data Env = Env
  { domains :: IORef (MonoidalHashMap Text (Sum Int)) -- ^ count the domains distribution
  , lang :: IORef (MonoidalHashMap Language (Sum Int)) -- ^ count the languages distribution
  }

-- |
-- > extractHostRoot "www.haskell.org"
-- "org"
extractHostRoot :: Text -> Text
extractHostRoot = last . T.splitOn "."

-- | Against the Bool blindeness in parameters for 'detectLanguage'
cld2_html :: Bool
cld2_html = False

iterFunc :: Env -> Record IO b -> IO b
iterFunc Env {..} Record {..} = do
  let mroot =
        recHeader ^. recHeaders . at "WARC-Target-URI" <&> S.toString <&>
        parseURI &
        join <&>
        uriAuthority &
        join <&>
        uriRegName <&>
        S.toText <&>
        extractHostRoot
  case mroot of
    Nothing -> return ()
    Just root -> modifyIORef domains (MH.modify (+ 1) root)
  let mIsHttp =
        recHeader ^. recHeaders . at "Content-Type" <&> S.toText <&>
        ("application/http" `T.isInfixOf`)
  case mIsHttp of
    Nothing -> skip
    Just False -> skip
    Just True -> do
      tmp <- newIORef T.empty
      r <-
        liftIO $ P.runEffect $ P.for recContent $ \x -> do
          liftIO (modifyIORef tmp (<> (S.toText x)))
      text <- readIORef tmp
      let isHtml = "Content-Type: text/html" `T.isInfixOf` (T.take 2000 text)
      case isHtml of
        False -> return r
        True -> do
          let (htmlHeaders, htmlBody) = getHtmlBody text
          let hints =
                defaultHints
                { hintContentLanguage =
                    List.lookup "Content-Language" htmlHeaders <&> S.toString
                , hintTLD = mroot <&> S.toString
                }
          let language = resultSimple (detectLanguage htmlBody cld2_html hints)
          modifyIORef lang (MH.modify (+ 1) language)
          return r
  where
    skip = do
      r <- liftIO $ P.runEffect $ P.for recContent $ \x -> x `seq` return ()
      return r
    getHtmlBody t =
      let lines' = T.lines t
          (headers, body) = List.span headerOrEmpty lines'
      in (mapMaybe parseHeader headers, T.unlines body)
    headerOrEmpty t =
      T.strip t == "" || ":" `T.isInfixOf` t || "HTTP" `T.isInfixOf` t
    parseHeader headerLine =
      case T.strip headerLine of
        "" -> Nothing
        l ->
          case T.span (== ':') l of
            (k, v) ->
              if T.strip k /= "" && T.strip v /= ""
                then Just (k, v)
                else Nothing

-- | See https://github.com/k0001/pipes-zlib/issues/29
decompressAll ::
     MonadIO m => P.Producer ByteString m r -> P.Producer ByteString m r
decompressAll p = do
  er <- PGZip.decompress' p
  case er of
    Left leftover -> decompressAll leftover
    Right r -> return r

getTopN :: Ord a => Int -> MonoidalHashMap k (Sum a) -> [(k, a)]
getTopN n mhm =
  take n (sortBy (flip compare `on` snd) (map (second getSum) (MH.toList mhm)))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [crawlFile]
      -- let crawlFile =
      --       "/home/kb/Downloads/commoncrawl/CC-MAIN-20180221222354-20180222002354-00249.warc.gz"
     -> do
      withFile crawlFile ReadMode $ \h -> do
        env <- Env <$> newIORef mempty <*> newIORef mempty
        _ <-
          iterRecords (iterFunc env) (parseWarc (decompressAll (fromHandle h)))
        doms <- readIORef (domains env)
        putStrLn "Top 10 domains:"
        putStrLn $ groom $ getTopN 10 doms
        putStrLn "Top 10 used languages:"
        languages <- readIORef (lang env)
        putStrLn $ groom $ getTopN 10 languages
        return ()
    _ ->
      putStrLn
        "Usage: stack build && stack exec common-crabl -- /path/to/some.warc.gz"

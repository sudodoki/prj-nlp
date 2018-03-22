{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Concurrent
import Control.Lens
import Control.Monad (forM_, unless, when)
import Data.FileEmbed (embedFile)
import Data.List (nub)
import Data.Maybe
import Data.Semigroup
import qualified Data.String.Class as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.Wreq
import System.Directory
import System.Environment
import Text.Groom
import Text.HTML.Scalpel.Core
import Text.RE.TDFA.Text

threadExample :: Text
threadExample = S.toText $(embedFile "data/thread_example.html")

threadExampleMultipage :: Text
threadExampleMultipage =
  S.toText $(embedFile "data/thread_example_multipage.html")

topicsListExample :: Text
topicsListExample = S.toText $(embedFile "data/topics_list_example.html")

-- | TODO: replace with an actual URL
type URL = Text

data Post = Post
  { postNick :: Text
  , postContent :: Text
  } deriving (Eq, Show)

data ThreadPage = ThreadPage
  { threadOtherPages :: [URL]
  , title :: Text
  , posts :: [Post]
  } deriving (Eq, Show)

data TLPost = TLPost
  { tlpTitle :: Text
  , tlpDate :: Text
  , tlpThreadLink :: URL
  } deriving (Eq, Show)

data TopicsListPage = TopicsListPage
  { tlOtherPages :: [URL]
  , tlPosts :: [TLPost]
  } deriving (Eq, Show)

data Job
  = DownloadTopicsListPage URL
  | DownloadThreadPage URL
  deriving (Show, Eq)

data JobResult
  = DownloadTopicsListRes TopicsListPage
  | DownloadThreadRes ThreadPage
  deriving (Show, Eq)

getJobUrl :: Job -> URL
getJobUrl (DownloadTopicsListPage u) = u
getJobUrl (DownloadThreadPage u) = u

showJobResult :: JobResult -> IO ()
showJobResult (DownloadTopicsListRes TopicsListPage {..}) = do
  forM_ tlPosts $ \TLPost {..} -> do
    S.putStrLn $ tlpDate <> " - " <> tlpTitle <> " - " <> tlpThreadLink
  putStrLn $ groom $ tlOtherPages
showJobResult (DownloadThreadRes ThreadPage {..}) = do
  S.putStrLn title
  putStrLn ""
  forM_ posts $ \Post {..} -> do
    S.putStrLn postNick
    putStrLn ""
    S.putStrLn (T.unlines (take 5 (T.lines postContent)))
    putStrLn ""
  putStrLn $ groom $ threadOtherPages

cleanUp :: Text -> Text
cleanUp = removeEmptyNewlines . removeAds
  where
    removeEmptyNewlines = T.unlines . filter (/= "") . map T.strip . T.lines
    removeAds t = t *=~/ [edBI|р е к л а м а.*-->///|]

parsePost :: Scraper Text Post
parsePost = do
  postNick <- text $ "a" @: [hasClass "username"]
  c <- text $ "div" @: [hasClass "messageContent"]
  let postContent = cleanUp c
  return Post {..}

parseThread :: Scraper Text ThreadPage
parseThread = do
  title <- text "title"
  posts <- chroots ("li" @: [hasClass "message"]) parsePost
  threadOtherPagesRaw <-
    chroots ("div" @: [hasClass "PageNav"] // "nav" // "a") (attr "href" "a")
  let threadOtherPages = nub (filter ((/= "") . T.strip) threadOtherPagesRaw)
  return ThreadPage {..}

testPost :: IO ()
testPost = do
  let mtp = scrapeStringLike threadExample parseThread
  case mtp of
    Nothing -> putStrLn "Couldn't parse a thread"
    Just tp -> showJobResult (DownloadThreadRes tp)

testPostMultipage :: IO ()
testPostMultipage = do
  let mtp = scrapeStringLike threadExampleMultipage parseThread
  case mtp of
    Nothing -> putStrLn "Couldn't parse a thread"
    Just tp -> showJobResult (DownloadThreadRes tp)

parseTLPost :: Scraper Text TLPost
parseTLPost = do
  tlpTitleRaw <- text $ "h3" @: [hasClass "title"] // "a"
  tlpThreadLink <- attr "href" $ "h3" @: [hasClass "title"] // "a"
  dateRaw <- text $ "div" @: [hasClass "posterDate"] // "span"
  let tlpDate = T.strip (dateRaw *=~/ [edBI|,///|])
  let tlpTitle = T.strip tlpTitleRaw
  return TLPost {..}

parseTopicsList :: Scraper Text TopicsListPage
parseTopicsList = do
  tlPosts <- chroots ("li" @: [hasClass "discussionListItem"]) parseTLPost
  tlOtherPagesRaw <-
    chroots ("div" @: [hasClass "PageNav"] // "nav" // "a") (attr "href" "a")
  let tlOtherPages = nub (filter ((/= "") . T.strip) tlOtherPagesRaw)
  return TopicsListPage {..}

testTopicsList :: IO ()
testTopicsList = do
  let mpl = scrapeStringLike topicsListExample parseTopicsList
  case mpl of
    Nothing -> putStrLn "Couldn't parse a topics list"
    Just tlp -> do
      showJobResult (DownloadTopicsListRes tlp)

downloadTopicsList :: CacheDir -> URL -> IO (JobResult, [Job])
downloadTopicsList cacheDir url = do
  htmlContent <- downloadUrlCached cacheDir url
  let mlp = scrapeStringLike htmlContent parseTopicsList
  case mlp of
    Nothing -> error "Couldn't parse a topic list"
    Just tl -> do
      let jobs = map DownloadThreadPage (tlOtherPages tl)
      return (DownloadTopicsListRes tl, jobs)

downloadThread :: CacheDir -> URL -> IO (JobResult, [Job])
downloadThread cacheDir url = do
  htmlContent <- downloadUrlCached cacheDir url
  let mt = scrapeStringLike htmlContent parseThread
  case mt of
    Nothing -> error "Couldn't parse a thread"
    Just thread -> do
      let jobs = map DownloadThreadPage (threadOtherPages thread)
      return (DownloadThreadRes thread, jobs)

type CacheDir = Text

type FileName = Text

urlToFilename :: URL -> FileName
urlToFilename = T.replace "/" "__" . T.replace "?" "--"

getFromCache :: CacheDir -> URL -> IO (Maybe Text)
getFromCache cacheDir url = do
  let fn = urlToFilename url
  let fullPath = cacheDir <> fn
  r <- doesFileExist (S.toString fullPath)
  case r of
    True -> Just <$> T.readFile (S.toString fullPath)
    False -> return Nothing

storeInCache :: CacheDir -> URL -> Text -> IO ()
storeInCache cacheDir url contents =
  let fn = urlToFilename url
      fullPath = cacheDir <> fn
  in T.writeFile (S.toString fullPath) contents

-- | Download if URL not already in cache
downloadUrlCached :: CacheDir -> URL -> IO Text
downloadUrlCached cacheDir url = do
  S.putStrLn ("Downloading: " <> url)
  mres <- getFromCache cacheDir url
  case mres of
    Nothing -> do
      putStrLn "Not in cache"
      bsr <- get ("http://forum.lvivport.com/" <> S.toString url)
      let t = bsr ^. responseBody . to S.fromLazyByteString
      storeInCache cacheDir url t
      threadDelay 1000000 -- 1 sec
      return t
    Just x -> do
      S.putStrLn ("Got file from cache: " <> url)
      return x

processJob :: CacheDir -> Job -> IO (JobResult, [Job])
processJob cacheDir (DownloadTopicsListPage url) =
  downloadTopicsList cacheDir url
processJob cacheDir (DownloadThreadPage url) = downloadThread cacheDir url

filterCached :: CacheDir -> [Job] -> IO [Job]
filterCached cacheDir us = do
  res <- catMaybes <$> mapM f us
  return res
  where
    f job = do
      res <- getFromCache cacheDir (getJobUrl job)
      return (maybe (Just job) (const Nothing) res)

-- | Main starting point
processJobs :: CacheDir -> [Job] -> IO ()
processJobs _ [] = return ()
processJobs cacheDir queue@(x:xs) = do
  putStrLn "Queue: "
  print (take 5 queue)
  when (length queue > 5) $ do
    putStrLn ("...and " ++ show ((length queue) - 5) ++ " more")
  (res, newJobs) <- processJob cacheDir x
  putStrLn "-----"
  putStrLn $ "Received a result:"
  showJobResult res
  putStrLn ""
  newJobsFiltered <- filterCached cacheDir newJobs
  processJobs cacheDir (xs ++ newJobsFiltered)

main :: IO ()
main = do
  [cacheDir] <- getArgs
  -- let cacheDir = "data/cache/"
  r <- doesDirectoryExist cacheDir
  unless r (createDirectory cacheDir)
  -- testTopicsList
  -- testPost
  -- testPostMultipage
  processJobs
    (S.toText cacheDir)
    [DownloadTopicsListPage "forums/filosofija-ta-moral.88/"]

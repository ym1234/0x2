{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Crypto.Hash.SHA256 (hashlazy)
import qualified Data.ByteString.Lazy as BS
import Data.HexString
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding
import Data.UnixTime
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Parse
import System.Directory
import System.Environment (getArgs)
import System.IO
import System.Posix.Files
import Web.Scotty

main :: IO ()
main = do
  x <- getArgs
  case x of
    [] -> putStrLn "Download directory wasn't provided, exiting..."
    (a:[]) -> putStrLn "URL wasn't provided, exiting..."
    (a:b:_) -> do
      createDirectoryIfMissing True a
      forkIO (forever $ threadDelay (toMicrosecond 1) >> cleanup a) -- clean up every hour
      run (TL.pack a) (TL.pack b)

toMicrosecond :: Int -> Int
toMicrosecond a = a * 1000000 * 60 * 60

fromMBtoB :: Int -> Int
fromMBtoB a = a * 1024 * 1024

-- min_age + (-max_age + min_age) * pow((file_size / max_size - 1), 3)
retention :: Integer -> Integer
retention a = (1 + (-365 + 1) * (a `div` (512 - 1)) ^ 3) * 24 * 60 * 60

cleanup :: String -> IO ()
cleanup a =
  putStrLn "Starting cleanup" >> getDirectoryContents a >>=
  void . sequence . fmap (checkFile . mappend a)

checkFile :: FilePath -> IO ()
checkFile x = do
  y <- getFileStatus x
  when (not $ isDirectory y || x == "." || x == "..") $ do
    filesize <- withFile x ReadMode hFileSize
    currentTime <- getUnixTime
    when
      (diffUnixTime currentTime (fromEpochTime $ modificationTime y) >
       secondsToUnixDiffTime (retention filesize)) $
      removeLink x

run :: TL.Text -> TL.Text -> IO ()
run a sitename =
  scotty 8080 $ do
    middleware logStdout
    get "/" $ text "hello world"
    post "/" $ files >>= liftIO . fmap mconcat . mapM upload >>= text -- Idk why mapM_ text only returns the last Text, so we concat them
    get "/:file" $ param "file" >>= file . TL.unpack . mappend a -- TODO set content type, can be guessed by browsers but meh
  where
    upload :: Web.Scotty.File -> IO TL.Text
    upload (_, fileinfo) =
      if (BS.length content > fromIntegral (fromMBtoB 512))
        then return $ "File size too large: " <> filename <> "\n"
        else doesFileExist path >>= flip unless (BS.writeFile path content) >>
             return (sitename <> h <> "\n")
      where
        content = fileContent fileinfo
        filename = decodeUtf8 $ BS.fromStrict $ fileName fileinfo
        h = TL.fromStrict . toText . fromBytes . hashlazy $ content
        path = TL.unpack $ a <> h

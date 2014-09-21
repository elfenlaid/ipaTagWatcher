import qualified System.FSNotify as FSN
import qualified Filesystem.Path.CurrentOS as FPC
import Prelude hiding (FilePath)

import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import qualified System.Directory as SD

import Data.Char (isAlpha, isSpace)
import Data.Text (pack, unpack)
import Data.Time (formatTime, getCurrentTime, utcToLocalZonedTime)
import System.Locale (defaultTimeLocale)

import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

import Control.Concurrent (threadDelay)
import Control.Monad (forever)

main :: IO a
main = do
  dir <- getArgs >>= parseArgs
  putStrLn $ "Start tag observer for directory " ++ dir

  FSN.withManager $ \mgr -> do
    FSN.watchDir
      mgr
      (FPC.fromText (pack dir))
      ipaFilePredicate
      ipaTagAction
    forever $ threadDelay 1000000000

parseArgs :: [String] -> IO String 
parseArgs [dir] = do 
  isExists <- SD.doesDirectoryExist dir
  case isExists of 
    True -> return dir
    False -> do 
      hPutStrLn stderr $ "directory " ++ dir ++ " doesn't exist"
      exitWith (ExitFailure 1)
parseArgs _ = do 
  hPutStrLn stderr "provide a path to the observable directory"
  exitWith (ExitFailure 1)

ipaFilePredicate :: FSN.Event -> Bool
ipaFilePredicate = isPlainIPA . FSN.eventPath
  where isPlainIPA path = isValidName (pathName path) && 
                          isValidExtension (pathExtension path)
        isValidName = all (\x -> any ($ x) [isAlpha, isSpace])
        isValidExtension = (== "ipa")

ipaTagAction :: FSN.Event -> IO ()
ipaTagAction e = do 
  print e
  case e of 
    FSN.Modified path _ -> tagFile path
    FSN.Added path _ -> tagFile path
    _ -> return ()
  where tagFile path = do 
          taggedPath <- fmap (appendTag path) timeTag 
          SD.copyFile (pathToString path) (pathToString taggedPath)
  
timeTag :: IO String
timeTag = fmap formatter $ getCurrentTime >>= utcToLocalZonedTime
  where formatter = formatTime defaultTimeLocale "(%Y-%m-%d__%H-%M)" 

appendTag :: FPC.FilePath -> String -> FPC.FilePath
appendTag path tag = dir `FPC.append` taggedName
  where dir = FPC.directory path
        taggedName = FPC.fromText $ pack $ pathName path ++ "_" ++ tag ++ "." ++ pathExtension path

pathName :: FPC.FilePath -> String 
pathName = pathToString . FPC.basename

pathExtension :: FPC.FilePath -> String
pathExtension = unpack . fromMaybe (pack "") . FPC.extension 

pathToString :: FPC.FilePath -> String
pathToString path = unpack $ case FPC.toText path of
                                    Left t -> t
                                    Right t -> t

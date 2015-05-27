{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text.ICU
import System.Environment
import System.Directory
import Control.Monad
import Data.Text.Format (Format(..), format)
import Data.Bool
import Data.Traversable (for)
import Data.Maybe (catMaybes)
import Data.Text.Lazy as Lazy.Text (Text, toStrict, unpack, pack)
import Options (Options, defineOption, optionLongFlags, optionShortFlags, optionType_string, optionDescription, defineOptions, runCommand, optionDefault, optionType_maybe)
import qualified Data.Text as T (pack, Text)
import qualified Data.Text.IO as TIO (putStrLn)
import Data.String as String (fromString)


noRegexMessage :: T.Text
noRegexMessage = "You need provide a regex with the '-r' or '--regex' option"


noFormatMessage :: T.Text
noFormatMessage = "you need provide an output format with the '-f' or '--format' option"


data CallArgs = CallArgs { workingDir   :: FilePath
                         , chosenRegex  :: Maybe String
                         , outputFormat :: Maybe String
                         }


instance Options CallArgs where
  defineOptions = CallArgs
    <$> defineOption
          optionType_string
          (\o -> o {
            optionLongFlags   = ["workingDir"],
            optionShortFlags  = "w",
            optionDescription = "Directory who's content is to be renamed",
            optionDefault     = "."
          })
    <*> defineOption
          (optionType_maybe optionType_string)
          (\o -> o {
            optionLongFlags   = ["regex"],
            optionShortFlags  = "r",
            optionDescription = "Regex to use for conversion (required)"
          })
    <*> defineOption
          (optionType_maybe optionType_string)
          (\o -> o {
            optionLongFlags   = ["format"],
            optionShortFlags  = "f",
            optionDescription = "Format string for conversion target (required)"
          })


rawR :: Either ParseError Regex
rawR = regex' [] "^Elementary.S(\\d+)E(\\d+).720p.HDTV.X264-DIMENSION.mkv$"


out :: Format
out = "Season {} Episode {}.mkv"


translateOne :: Regex -> Format -> Text -> Maybe Text
translateOne r out = fmap (format out . tail . unfold group) . find r . toStrict


renameOne :: Regex -> Format -> FilePath -> IO ()
renameOne r out = maybe (return ()) <$> renameFile <*> fmap unpack . translateOne r out . pack


regexSuccess :: Regex -> Format -> [FilePath] -> IO ()
regexSuccess =  (mapM_ .) . renameOne


main' :: CallArgs -> [String] -> IO()
main' opts args = do
  maybe
    (TIO.putStrLn noRegexMessage)
    (\rawRegex ->
      maybe
        (TIO.putStrLn noFormatMessage)
        (\rawFormat -> do

          files <- getDirectoryContents (workingDir opts) >>= filterM doesFileExist

          mapM putStrLn files

          putStrLn "\n\n"

          either
            (putStrLn . ("unusable Regex " ++) . show)
            (flip (flip regexSuccess $ uFormat rawFormat) files)
            (regex' [] $ T.pack rawRegex)

          return ())
        (outputFormat opts))
    (chosenRegex opts)
  where
    uFormat :: String -> Format
    uFormat = String.fromString



main :: IO ()
main = runCommand main'

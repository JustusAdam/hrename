{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Text.ICU
import           System.Environment
import           System.Directory
import           System.FilePath    ((</>))
import           Control.Monad
import           Data.Text.Format   (Format(..), format)
import           Data.Bool
import           Data.Traversable   (for)
import           Data.Maybe         (catMaybes)
import           Data.Text.Lazy     as Lazy.Text (Text, toStrict, unpack, pack)
import           Options            (Options, defineOption, optionLongFlags,
                                    optionShortFlags,  optionType_string,
                                    optionDescription, defineOptions, runCommand,
                                    optionDefault, optionType_maybe, optionType_bool)
import qualified Data.Text          as T (pack, Text, append)
import qualified Data.Text.IO       as TIO (putStrLn)
import           Data.String        as String (fromString)


noRegexMessage :: T.Text
noRegexMessage = "You need provide a regex with the '-r' or '--regex' option"


noFormatMessage :: T.Text
noFormatMessage = "you need provide an output format with the '-f' or '--format' option"


invalidDir :: FilePath -> T.Text
invalidDir p = "Directory '" `T.append` T.pack p `T.append` "' does not exist"


data CallArgs = CallArgs { workingDir    :: FilePath
                         , chosenRegex   :: Maybe String
                         , outputFormat  :: Maybe String
                         , scanRecursive :: Bool
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
    <*> defineOption
          (optionType_bool)
          (\o -> o {
            optionShortFlags  = "r",
            optionLongFlags   = ["recursive"],
            optionDescription = "Scan subdirectories recursively, applying the renaming (untested)"
          })


translateOne :: Regex -> Format -> Text -> Maybe Text
translateOne r out = fmap (format out . tail . unfold group) . find r . toStrict


renameOne :: Regex -> Format -> FilePath -> IO ()
renameOne r out = maybe (return ()) <$> renameFile <*> fmap unpack . translateOne r out . pack


regexSuccess :: Bool -> Regex -> Format -> FilePath -> IO ()
regexSuccess rec r f d =
   files >>=
    filterM doesFileExist >>=
      mapM_ (renameOne r f) >> bool (return ()) doItAgain rec
  where
    files = fmap (map (d </>)) (getDirectoryContents d)

    doItAgain :: IO ()
    doItAgain = files >>= filterM doesDirectoryExist >>= mapM_ (regexSuccess True r f)


main' :: CallArgs -> [String] -> IO()
main' opts args = do
  maybe
    (TIO.putStrLn noRegexMessage)
    (\rawRegex ->
      maybe
        (TIO.putStrLn noFormatMessage)
        (\rawFormat -> do
          (doesDirectoryExist (workingDir opts) >>=
            bool
              (TIO.putStrLn (invalidDir (workingDir opts)))
              (either
                (putStrLn . ("unusable Regex " ++) . show)
                (\r -> regexSuccess (scanRecursive opts) r (uFormat rawFormat) (workingDir opts))
                (regex' [] $ T.pack rawRegex))))
        (outputFormat opts))
    (chosenRegex opts)
  where
    uFormat :: String -> Format
    uFormat = String.fromString



main :: IO ()
main = runCommand main'

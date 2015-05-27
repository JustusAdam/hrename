{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Text.ICU      (regex, Regex, unfold, group, find, )
import           System.Directory   (doesFileExist, doesDirectoryExist,
                                    renameFile, getDirectoryContents)
import           System.FilePath    ((</>))
import           Control.Monad      (filterM)
import           Data.Text.Format   (Format, format)
import           Data.Text.Lazy     as Lazy.Text (Text, toStrict, unpack, pack)
import           Options            (Options, defineOption, optionLongFlags,
                                    optionShortFlags,  optionType_string,
                                    optionDescription, defineOptions, runCommand,
                                    optionDefault, optionType_maybe,
                                    optionType_bool)
import qualified Data.Text          as T (pack, Text, append)
import qualified Data.Text.IO       as TIO (putStrLn)
import           Data.String        (fromString)


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
            optionShortFlags  = "e",
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
          optionType_bool
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
regexSuccess isRecusive r f d = do
  allRealFiles >>= mapM_ (renameOne r f)
  if isRecusive
    then doItAgain
    else return ()
  where
    files = fmap (map (d </>)) (getDirectoryContents d)

    allRealFiles = files >>= filterM doesFileExist
    allRealDirs  = files >>= filterM doesDirectoryExist

    doItAgain :: IO ()
    doItAgain = allRealDirs >>= mapM_ (regexSuccess True r f)


main' :: CallArgs -> [String] -> IO()
main' opts _ =
  maybe
    (TIO.putStrLn noRegexMessage)
    (\rawRegex ->
      maybe
        (TIO.putStrLn noFormatMessage)
        (\rawFormat ->
          doesDirectoryExist (workingDir opts) >>=
          (\dirExists ->
            if dirExists
              then
                let
                  isRecursive      = scanRecursive opts
                  compiledRegex    = regex [] $ T.pack rawRegex
                  formatString     = fromString rawFormat
                  workingDirectory = workingDir opts
                in
                regexSuccess isRecursive compiledRegex formatString workingDirectory
            else
              TIO.putStrLn (invalidDir (workingDir opts))))
        (outputFormat opts))
    (chosenRegex opts)



main :: IO ()
main = runCommand main'

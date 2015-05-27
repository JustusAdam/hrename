{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Text.ICU      (regex, Regex, unfold, group, find, )
import           System.Directory   (doesFileExist, doesDirectoryExist,
                                    renameFile, getDirectoryContents,
                                    renameDirectory)
import           System.FilePath    ((</>))
import           Control.Monad      (filterM)
import           Data.Text.Format   (Format, format)
import           Data.Bool          (bool)
import           Data.Text.Lazy     as Lazy.Text (Text, toStrict, unpack, pack)
import           Options            (Options, defineOption, optionLongFlags,
                                    optionShortFlags,  optionType_string,
                                    optionDescription, defineOptions, runCommand,
                                    optionDefault, optionType_maybe,
                                    optionType_bool)
import qualified Data.Text          as T (pack, Text, append)
import qualified Data.Text.IO       as TIO (putStrLn)
import           Data.String        (fromString)


noRegexMessage  :: T.Text
noRegexMessage  = "You need provide a regex with the '-e' or '--regex' option"


noFormatMessage :: T.Text
noFormatMessage = "you need provide an output format with the '-f' or '--format' option"


invalidDir      :: FilePath -> T.Text
invalidDir p    = "Directory '" `T.append` T.pack p `T.append` "' does not exist"


data CallArgs = CallArgs { workingDir         :: FilePath
                         , chosenRegex        :: Maybe String
                         , outputFormat       :: Maybe String
                         , scanRecursive      :: Bool
                         , excludeFiles       :: Bool
                         , excludeDirs        :: Bool
                         }


instance Options CallArgs where
  defineOptions = CallArgs
    <$> defineOption
          optionType_string
          (\o -> o {
            optionLongFlags   = ["directory"],
            optionShortFlags  = "d",
            optionDescription = "Directory who's content is to be renamed",
            optionDefault     = "."
          })
    <*> defineOption
          (optionType_maybe optionType_string)
          (\o -> o {
            optionLongFlags   = ["regex"],
            optionShortFlags  = "e",
            optionDescription = "Regex to use for conversion (required)\n    " ++
            "the regex matcher utilizes the icu standard, " ++
            "a current copy of which can be found here: http://userguide.icu-project.org/strings/regexp"
          })
    <*> defineOption
          (optionType_maybe optionType_string)
          (\o -> o {
            optionLongFlags   = ["format", "output-format"],
            optionShortFlags  = "f",
            optionDescription = "Format string for conversion target (required)"
          })
    <*> defineOption
          optionType_bool
          (\o -> o {
            optionShortFlags  = "r",
            optionLongFlags   = ["recursive", "scan-recursive"],
            optionDescription = "Scan subdirectories recursively, applying the renaming (untested)"
          })
    <*> defineOption
          optionType_bool
          (\o -> o {
            optionLongFlags   = ["exclude-files"],
            optionShortFlags  = "x",
            optionDescription = "Do not rename files"
          })
    <*> defineOption
          optionType_bool
          (\o -> o {
            optionLongFlags   = ["exclude-dirs"],
            optionShortFlags  = "n",
            optionDescription = "Do not rename directories"
          })



translateOne :: Regex -> Format -> Text -> Maybe Text
translateOne r out = fmap (format out . tail . unfold group) . find r . toStrict


renameOne :: Bool -> Bool -> Regex -> Format -> FilePath -> IO ()
renameOne doFiles doDirs r out =
  maybe (return ()) <$> rename <*> fmap unpack . translateOne r out . pack
  where
    rename s t =
      tryFile s t >>=
      bool
        (tryDir s t >> return ())
        (return ())

    try :: Monad m => Bool -> (b -> m Bool) -> (b -> c -> m a) -> b -> c -> m Bool
    try dec finder action s t =
      bool
        (return False)
        (finder s >>= bool (return False) (action s t >> return True))
        dec

    tryFile = try doFiles doesFileExist renameFile
    tryDir = try doDirs doesDirectoryExist renameDirectory


regexSuccess :: Bool -> Bool -> Bool -> Regex -> Format -> FilePath -> IO ()
regexSuccess doFiles doDirs isRecusive r f d =
  if isRecusive
    then doItAgain
    else return ()
  >>
  files >>= mapM_ (renameOne doFiles doDirs r f)
  where
    files        = fmap (map (d </>)) (getDirectoryContents d)
    allRealDirs  = files >>= filterM doesDirectoryExist

    doItAgain    :: IO ()
    doItAgain    = allRealDirs >>= mapM_ (regexSuccess doFiles doDirs True r f)


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
                  doFiles          = not (excludeFiles opts)
                  doDirs           = not (excludeDirs opts)
                in
                  regexSuccess doFiles doDirs isRecursive compiledRegex formatString workingDirectory
              else
                TIO.putStrLn (invalidDir (workingDir opts))))
        (outputFormat opts))
    (chosenRegex opts)



main :: IO ()
main = runCommand main'

{-# LANGUAGE OverloadedStrings #-}

module OptionsParser where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Text(Text, pack)
import Filesystem.Path.CurrentOS ((<.>), (</>))
import Data.String
import System.IO.Unsafe(unsafePerformIO)
import System.Directory(getAppUserDataDirectory)


data Options = Options {
    interactive :: Bool
  , template :: String
  , fileTemplate :: Maybe String
  , variableTerm :: [(Text, Text)]
  , command :: [String]
  , dryRun :: Bool
  , saveAs :: Maybe String
}
  deriving (Eq, Show, Read)

newtype EditCommand = EditCommand {
    file :: String
}
  deriving (Eq, Show, Read)

data JinsubOptions = Main Options | Subedit EditCommand


appName :: (IsString a) => a
appName = "jinsub"

addConfigExtension a = a <.> "jinsub"

{-# NOINLINE homePath #-}
homePath = unsafePerformIO $ fmap fromString (getAppUserDataDirectory appName)

getTemplatePath n = addConfigExtension (homePath </> n)


options :: Parser Options
options = Options
            <$> parseInter
            <*> parseTemplate
            <*> parseFileTemplate
            <*> parseVariableTerm
            <*> parseCommand
            <*> parseDryRun
            <*> parseSave
  where
    parseInter = switch (foldr1 (<>)
                          [long "interactive"
                          , short 'i'
                          , help "Whether display stdout in real time"])
    parseDryRun = switch (foldr1 (<>)
                          [long "dry-run"
                          , help "Print generated pbs job file to stdout without submitting to nodes"])
    parseTemplate = strOption (foldr1 (<>)
                                [long "template"
                                , short 't'
                                , metavar "TEMPLATE_NAME"
                                , help "PBS template name to use"
                                , value "default"
                                , showDefault
                                ])
    parseFileTemplate = optional $ strOption (foldr1 (<>)
                                    [long "file"
                                    , short 'f'
                                    , metavar "TEMPLATE_FILE"
                                    , help "Specify a template file path"])

    parseSave = optional $ strOption (foldr1 (<>)
                                      [long "save-as"
                                      , short 's'
                                      , metavar "FILEPATH"
                                      , help "Save generated pbs file to FILEPATH"
                                      ])

    parseVariableTerm = many $ option
                                (maybeReader variableTerm)
                                (foldr1 (<>)
                                  [short 'v'
                                  , metavar "VAR=VALUE"
                                  , help "Environment variables to set in template defination placeholder"
                                  ]
                                )



          where
        variableTerm :: String -> Maybe (Text, Text)
        variableTerm s =
          let (var, val) = span (/= '=') s in
            if null var || val == "=" || null val
              then Nothing
              else Just (pack var, pack (tail val))


    parseCommand = many (strArgument (metavar "CMD" <> help "Command to run in pbs job"))

editCommand :: Parser EditCommand
editCommand =
  subparser (Options.Applicative.command "edit" parseEdit)
    where
      parseEdit = info (EditCommand <$> parseNew) (progDesc "Edit a template file")
      parseNew = strArgument (metavar "TEMPLATE" <> help "")

jinsubOptions :: Parser JinsubOptions
jinsubOptions = foldr1 (<|>)
                  [jinsubEdit, jinsubMain]
  where
    jinsubEdit = Subedit <$> editCommand
    jinsubMain = Main <$> options


getOptions :: IO JinsubOptions
getOptions = execParser (info (jinsubOptions <**> helper) (progDesc "Quickly submit a pbs job"))

module OptionsParser where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Text(Text, pack)


data Options = Options {
    interactive :: Bool
  , template :: String
  , fileTemplate :: Maybe String
  , variableTerm :: [(Text, Text)]
  , command :: [String]
  , dryRun :: Bool
}
  deriving (Eq, Show, Read)

options :: Parser Options
options = Options
            <$> parseInter
            <*> parseTemplate
            <*> parseFileTemplate
            <*> parseVariableTerm
            <*> parseCommand
            <*> parseDryRun
  where
    parseInter = switch (foldr1 (<>)
                          [long "interactive"
                          , short 'i'
                          , help "Whether display stdout in real time"])
    parseDryRun = switch (foldr1 (<>)
                          [long "dry-run"
                          , help "Print generated pbs file to stdout submitting to nodes"])
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

    parseVariableTerm = many $ option
                                (maybeReader variableTerm)
                                (foldr1 (<>)
                                  [short 'v'
                                  , metavar "VAR=VALUE"
                                  , help "Environment variables to set in template defination placeholder"
                                  ])
      where
        variableTerm :: String -> Maybe (Text, Text)
        variableTerm s =
          let (var, val) = span (/= '=') s in
            if null var || val == "=" || null val
              then Nothing
              else Just (pack var, pack (tail val))


    parseCommand = many (strArgument (metavar "commands" <> help "Command to run in pbs job"))


getOptions :: IO Options
getOptions = execParser (info (options <**> helper) (progDesc "Quickly submit a pbs job"))

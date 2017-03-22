module OptionsParser where

import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options {
    interactive :: Bool
  , template :: String
  , fileTemplate :: Maybe String
  , command :: [String]
}
  deriving (Eq, Show, Read)

options :: Parser Options
options = Options
            <$> parseInter
            <*> parseTemplate
            <*> parseFileTemplate
            <*> parseCommand
  where
    parseInter = switch (foldr1 (<>)
                          [long "interactive"
                          , short 'i'
                          , help "Whether display stdout in real time"])
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
    parseCommand = some (strArgument (metavar "commands" <> help "Command to run in pbs job"))


getOptions :: IO Options
getOptions = execParser (info (options <**> helper) (progDesc "Quickly submit a pbs job"))

module OptionsParser where

import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options {
    interactive :: Bool
  , template :: String
  , command :: [String]
}

options :: Parser Options
options = Options <$> parseInter <*> parseTemplate <*> parseCommand
  where
    parseInter = switch (foldr1 (<>)
                          [long "interactive"
                          , short 'i'
                          , help "Whether display stdout in real time"])
    parseTemplate = strOption (foldr1 (<>)
                                [long "template"
                                , short 't'
                                , metavar "TEMPLATE"
                                , help "PBS template to use"
                                , value "default"
                                ])
    parseCommand = some (strArgument (metavar "commands" <> help "Command to run in pbs job"))


getOptions :: IO Options
getOptions = execParser (info (options <**> helper) (progDesc "Quickly submit a pbs job"))

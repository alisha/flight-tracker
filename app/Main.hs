module Main where

import Lib

import Options.Applicative

data Opts = Opts
  { optionalFlag :: Maybe Int
  , boolwitch    :: Bool
  }

opts :: Parser Opts
opts = Opts
  <$> optional (option auto
    (  long "optional-flag"
    <> short 'f'
    <> metavar "VAL_NAME"
    <> help "optional flag-provided value" ))
  <*> switch
    (  long "bool-switch"
    <> help "boolean option flag" )

fullopts :: ParserInfo Opts
fullopts = info (helper <*> opts)
  (  fullDesc
  <> header "flight tracker -- real time status updates in your terminal" )

main :: IO ()
main = do
  (Opts _ _) <- execParser fullopts           -- get CLI opts/args
  runApp                                             -- play game


runApp :: IO ()
runApp = runApplication

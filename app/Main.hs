module Main where

import Control.Monad (when)
import System.Exit (exitSuccess)
import Text.Read (readMaybe)

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
  (Opts flg switch) <- execParser fullopts           -- get CLI opts/args
  exitCode <- runApp                                   -- play game
  putStrLn ("Thanks for playing")


runApp = error "unimplemented"

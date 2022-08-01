{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Options.Applicative
import System.FilePath ((</>))

import Data.Version (showVersion)
import qualified Hearts.Server as Server
import Paths_hearts (getDataDir, version)

main :: IO ()
main = do
  Options{port, stateFile} <- execParser opts
  staticDir <- fmap (</> "static") getDataDir
  Server.runServer staticDir port stateFile
  where
    opts =
      info
        (options <**> helper)
        ( fullDesc
            <> progDesc "Run a server to play the card game hearts on"
            <> header ("Hearts v" <> showVersion version)
        )

data Options = Options
  { port :: Int
  , stateFile :: Maybe FilePath
  }

options :: Parser Options
options =
  Options
    <$> option
      auto
      ( long "port"
          <> help "Port number to run the server on"
          <> showDefault
          <> value 9999
          <> metavar "N"
      )
    <*> option
      (Just <$> str)
      ( long "stateFile"
          <> help "State file to load at startup"
          <> value Nothing
          <> metavar "/path/to/state.json"
      )

{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Options.Applicative

import Data.Version (showVersion)
import qualified Hearts.Server as Server
import Paths_hearts (version)

main :: IO ()
main = do
  Options{port} <- execParser opts
  putStrLn ("Hearts server starting on port " <> show port)
  Server.runServer port
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

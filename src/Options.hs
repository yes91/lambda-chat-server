module Options
  ( Options(..)
  , getOpts
  ) where

import Network.Socket (ServiceName)

import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options { port :: ServiceName }

options :: Parser Options
options = Options
      <$> option str
          ( long "port"
         <> short 'p'
         <> help "The port at which to run the chat service."
         <> showDefault
         <> value "3000"
         <> metavar "PORT" )

fullOpts :: ParserInfo Options
fullOpts = info (options <**> helper)
         ( fullDesc
        <> progDesc "Run a chat server at port PORT"
        <> header "lambda-chat-server - a Haskell server implementation for the lambda-chat protocol." )

getOpts :: IO Options
getOpts = execParser fullOpts

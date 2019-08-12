-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module IdeDebugDriver (main) where

import Control.Applicative.Combinators
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Language.Haskell.LSP.Test as LSP
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types hiding (Command)
import Language.Haskell.LSP.Types.Lens
import qualified Language.Haskell.LSP.Types.Lens as LSP
import Options.Applicative

-- | We all love programming in YAML, don’t we? :)
data Command
    = OpenFile FilePath
    | CloseFile FilePath
    | WaitForCompletion
    | Delay Int -- delay in seconds
    | Repeat Int [Command]
    deriving Show

instance FromJSON Command where
    parseJSON = withObject "Command" $ \o -> do
        cmd <- o .: "cmd"
        case cmd :: T.Text of
            "open" -> OpenFile <$> o .: "file"
            "close" -> CloseFile <$> o.: "file"
            "wait" -> pure WaitForCompletion
            "repeat" -> Repeat <$> o .: "count" <*> o .: "cmds"
            "delay" -> Delay <$> o .: "seconds"
            _ -> fail $ "Unknown command " <> show cmd

data SessionConfig = SessionConfig
    { ideShellCommand :: String
    , ideRoot :: FilePath
    , ideCommands :: [Command]
    } deriving Show

instance FromJSON SessionConfig where
    parseJSON = withObject "SessionConfig" $ \o ->
        SessionConfig
             <$> o .: "ide-cmd"
             <*> o .: "project-root"
             <*> o .: "commands"

data Opts = Opts
    { optConfigPath :: FilePath
    , optVerbose :: Verbose
    } deriving Show

newtype Verbose = Verbose Bool
    deriving Show

optsInfo :: ParserInfo Opts
optsInfo = info (parser <**> helper) fullDesc
  where
    parser = Opts
        <$> strOption (long "config" <> short 'c' <> metavar "FILE" <> help "Path to config file")
        <*> flag (Verbose False) (Verbose True) (long "verbose" <> short 'v' <> help "Enable verbose output")

main :: IO ()
main = do
    opts <- execParser optsInfo
    conf <- Yaml.decodeFileThrow (optConfigPath opts)
    runSession (optVerbose opts) (conf :: SessionConfig)

damlLanguageId :: String
damlLanguageId = "daml"

runSession :: Verbose -> SessionConfig -> IO ()
runSession (Verbose verbose) SessionConfig{..} =
    LSP.runSessionWithConfig cnf ideShellCommand LSP.fullCaps ideRoot $ traverse_ interpretCommand ideCommands
    where cnf = LSP.defaultConfig { LSP.logStdErr = verbose, LSP.messageTimeout = 600 }

progressStart :: LSP.Session ProgressStartNotification
progressStart = do
    NotProgressStart msg <- LSP.satisfy $ \case
        NotProgressStart _ -> True
        _ -> False
    pure msg

progressDone :: LSP.Session ProgressDoneNotification
progressDone = do
    NotProgressDone msg <- LSP.satisfy $ \case
        NotProgressDone _ -> True
        _ -> False
    pure msg

interpretCommand :: Command -> LSP.Session ()
interpretCommand = \case
    OpenFile f -> void $ LSP.openDoc f damlLanguageId
    CloseFile f -> do
        uri <- LSP.getDocUri f
        LSP.closeDoc (TextDocumentIdentifier uri)
    WaitForCompletion -> do
        start <- progressStart
        skipManyTill LSP.anyMessage $ do
            done <- progressDone
            guard $ done ^. params . LSP.id == start ^. params . LSP.id
    Repeat count cmds -> replicateM_ count $ traverse_ interpretCommand cmds
    Delay seconds -> liftIO $ threadDelay (10 ^ (6 :: Int) * seconds)
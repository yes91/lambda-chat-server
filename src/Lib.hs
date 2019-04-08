module Lib
    ( appEntry
    ) where

import Network.Socket
import System.IO
import Control.Concurrent hiding(yield)
import Control.Concurrent.STM
import Data.Function (fix)
import Data.Maybe
import Control.Monad
import Control.Exception

import Pipes
import qualified Pipes.Prelude as P
import Pipes.Network.TCP (fromSocket, toSocket)
import Pipes.Binary (DecodingError(..), decode, encode)
import Pipes.Parse (parsed)
import Chat.Protocol

import Control.Monad.Reader
import Control.Monad.State

import App

type App = ReaderT Env (ReaderT Client (StateT Conversation IO))
type Server = Pipe ClientMsg ServerMsg App

appEntry :: IO ()
appEntry = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  putStrLn "Listening on port 4242..."
  env <- mkEnv
  mainLoop sock env

runStack :: Env -> Client -> Effect App a -> IO (a, Conversation)
runStack env client = flip runStateT (Conversation Nothing) 
                    . flip runReaderT client
                    . flip runReaderT env
                    . runEffect

mainLoop :: Socket -> Env -> IO ()
mainLoop sock env = do
  (conn, peer) <- accept sock
  putStrLn $ "Accepted connection from: " ++ show peer

  client <- mkClient
  atomically $ addClient env client

  let
    bytesReciever = fromSocket conn 4096
    decoder = parsed decode bytesReciever
    
    clientThread producer = do
      
      upChan <- newChan

      let sink = writeChan upChan
      
      upstream <- forkIO . runEffect $ lift (readChan upChan) >~ toSocket conn

      reader <- forkIO $ do
        let getMsg = atomically . readTChan $ sendChan client
            f (Msg nick msg) = MessageFrom nick msg
        runEffect $ do
          lift getMsg >~ P.map f >-> for cat encode >-> P.mapM_ sink
      
      -- (e, rest)
      ((e, _), Conversation rID) <- runStack env client $ do
        producer >-> P.mapM handleClient >-> for P.concat encode >-> P.mapM_ (liftIO . sink)

      flip runReaderT env $ maybe (return ()) (flip setBusy False) rID
      
      print e
      killThread upstream
      killThread reader
      -- TODO: Implement error recovery
      -- clientThread $ parsed decode rest
      
    finalize = do
      putStrLn $ "Closing connection to: " ++ show peer
      atomically $ removeClient env client
      close conn
  
  forkFinally (clientThread decoder) (const finalize)
    
  mainLoop sock env

handleClient :: ClientMsg -> App (Maybe ServerMsg)
handleClient RequestJoin = return $ Just ConfirmJoin
handleClient (CheckName name) = Just . NameTaken <$> nameTaken name
handleClient (RegisterName name) = do
  cID <- lift (asks ident)
  setName cID name
  return Nothing
handleClient GetClients = Just . ActiveClients <$> getNames
handleClient (ChooseRecip name) = do
  rID <- getID name
  put $ Conversation rID
  maybe (return ()) (flip setBusy True) rID
  return Nothing
handleClient (Message msg) = do
  when (msg == ".exit") $ fail "Connection closed!"
  cID <- lift (asks ident)
  recip <- gets recipient
  nick <- maybe "" id <$> getName cID
  sent <- case recip of
    Just i -> sendMessage i $ Msg nick msg
    Nothing -> return False
  return Nothing
-- handleClient _ = fail "Unhandled request"

{-
handleClient :: Env -> Client ->  IO ()
handleClient env@Env{..} client@Client{..} = do
  
   hPutStrLn handle $ "Welcome " ++ nick ++ "!"

   recip <- fix $ \loop -> do
     hPutStrLn handle $ "Here is a list of available users: "
     users <- atomically $ Map.keys <$> readTVar clients
     mapM_ (hPutStrLn handle) users
     hPutStrLn handle "Please choose a recipient: "
     choice <- hGetLine handle
     result <- atomically $ do
       exists <- clientExists env choice
       busy <- isBusy env choice
       return $ exists && not busy
     if result
       then atomically (setBusy env choice True) >> return choice
       else hPutStrLn handle "Invalid recipient, try again." >> loop

   reader <- forkIO . forever $ do
     msg <- atomically $ readTChan sendChan
     hPutStrLn handle msg

   fix $ \loop -> do
     input <- hGetLine handle
     when (input /= ".exit") $ do
       success <- atomically $ sendMessage env recip input
       if success
         then loop
         else hPutStrLn handle "Send failed, recipient disconnected."

   killThread reader
   atomically (setBusy env recip False)
-}
   
       
     








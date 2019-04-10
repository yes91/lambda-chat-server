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
  env <- mkEnv
  addr <- resolve "3000"
  putStrLn "Listening on port 3000..."
  sock <- open addr
  mainLoop sock env
  where
    resolve port = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        -- If the prefork technique is not used,
        -- set CloseOnExec for the security reasons.
        -- fd <- fdSocket sock
        -- setCloseOnExecIfNeeded fd
        bind sock (addrAddress addr)
        listen sock 10
        return sock

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
            f (Req rID) = ChatRequest rID
            f (Msg pair msg) = MessageFrom pair msg
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

getNick :: App (ID, Name)
getNick = do
  cID <- lift (asks ident)
  maybe (cID, "") (\nick -> (cID, nick)) <$> getName cID

setNick :: Name -> App ()
setNick name = do
  cID <- lift (asks ident)
  setName cID name

handleClient :: ClientMsg -> App (Maybe ServerMsg)
handleClient RequestJoin = return $ Just ConfirmJoin
handleClient (CheckName name) = Just . NameTaken <$> nameTaken name
handleClient (RegisterName name) = setNick name >> return Nothing
handleClient GetClients = do
  cID <- lift (asks ident)
  Just . ActiveClients <$> getNames cID
handleClient (ChooseRecip rID) = do
  cID <- lift (asks ident)
  put . Conversation $ Just rID
  setBusy rID True
  sendMessage rID $ Req cID
  return Nothing
handleClient (Message msg) = do
  when (msg == ".exit") $ fail "Connection closed."
  nick <- getNick
  recip <- gets recipient
  sent <- case recip of
    Just i -> sendMessage i $ Msg nick msg
    Nothing -> return False
  return $ if not sent then Just (Notice "Send failed, recipient disconnected.") else Nothing

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
   
       
     








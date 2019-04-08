module App
  ( mkEnv
  , mkClient
  , sendMessage
  , nameTaken
  , setName
  , getName
  , getNames
  , getID
  , isBusy
  , setBusy
  , addClient
  , removeClient
  , Env(..)
  , Name
  , ID
  , Message(..)
  , Client(..)
  , Conversation(..)
  ) where

import Network.Socket
import System.IO
import System.Random
import Control.Concurrent.STM
import Control.Monad
import Control.Applicative
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.Bimap (Bimap)
import qualified Data.Bimap as B

import Control.Monad.Reader
import Control.Monad.IO.Class

type Name = String
type ID = Int
data Message = Msg Name String

data Client = Client { ident :: ID
                     , sendChan :: TChan Message
                     , busy :: TVar Bool
                     }

sendClient :: Client -> Message -> STM ()
sendClient client msg = writeTChan (sendChan client) msg

sendMessage :: (MonadIO m, MonadReader Env m) => ID -> Message -> m Bool
sendMessage n msg = do
  env <- ask
  liftIO . atomically $ sendMessageT env n msg

sendMessageT :: Env -> ID -> Message -> STM Bool
sendMessageT Env{..} n msg = do
  mclient <- Map.lookup n <$> readTVar clients
  case mclient of
    Just client -> sendClient client msg >> return True
    Nothing -> return False

mkClient :: IO Client
mkClient = Client <$> randomIO <*> newTChanIO <*> newTVarIO False

data Env = Env
  { names :: TVar (Bimap ID Name)
  , clients :: TVar (IntMap Client)
  }

mkEnv :: IO Env
mkEnv = Env <$> newTVarIO B.empty <*> newTVarIO Map.empty

data Conversation = Conversation { recipient :: Maybe ID }

getName :: (MonadIO m, MonadReader Env m) => ID -> m (Maybe Name)
getName n = do
  ns <- asks names
  liftIO . atomically $ B.lookup n <$> readTVar ns

setName :: (MonadIO m, MonadReader Env m) => ID -> Name -> m ()
setName n name = do
  env <- ask
  liftIO . atomically $ setNameT env n name

setNameT :: Env -> ID -> Name -> STM ()
setNameT Env{..} n name = modifyTVar' names $ B.insert n name

nameTaken :: (MonadIO m, MonadReader Env m) => Name -> m Bool
nameTaken name = do
  env <- ask
  liftIO . atomically $ nameTakenT env name

nameTakenT :: Env -> Name -> STM Bool
nameTakenT Env{..} name = B.memberR name <$> readTVar names

getNames :: (MonadIO m, MonadReader Env m) => m [String]
getNames = do
  ns <- asks names
  liftIO . atomically $ B.elems <$> readTVar ns

getID :: (MonadIO m, MonadReader Env m) => Name -> m (Maybe ID)
getID name = do
  ns <- asks names
  liftIO . atomically $ B.lookupR name <$> readTVar ns

isBusy :: (MonadIO m, MonadReader Env m) => ID -> m Bool
isBusy n = do
  env <- ask
  liftIO . atomically $ isBusyT env n

isBusyT :: Env -> ID -> STM Bool
isBusyT Env{..} n = do
  mclient <- Map.lookup n <$> readTVar clients
  case mclient of
    Just client -> readTVar $ busy client
    Nothing -> return True

setBusy :: (MonadIO m, MonadReader Env m) => ID -> Bool -> m ()
setBusy n status = do
  env <- ask
  liftIO . atomically $ setBusyT env n status

setBusyT :: Env -> ID -> Bool -> STM ()
setBusyT Env{..} n status = do
  mclient <- Map.lookup n <$> readTVar clients
  case mclient of
    Just client -> writeTVar (busy client) status
    Nothing -> return ()

addClient :: Env -> Client -> STM ()
addClient Env{..} client = modifyTVar' clients $ Map.insert (ident client) client

removeClient :: Env -> Client -> STM ()
removeClient Env{..} client = do
  let n = ident client
  modifyTVar' names $ B.delete n
  modifyTVar' clients $ Map.delete n

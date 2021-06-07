{-# language BlockArguments, NamedFieldPuns, PartialTypeSignatures #-}
module Pure.Media.Library.GHC.Library (Config(..),library) where

import Pure.Media.Library.API as API
import Pure.Media.Library.Data.Library hiding (library)
import Pure.Media.Library.Data.Media

import Pure.Data.Txt (Txt,FromTxt(..))
import Pure.Data.Time (time)
import Pure.WebSocket as WS
import Pure.ReadFile (writeByteTxt)
import Sorcerer

import System.FilePath (takeDirectory)
import System.Directory (createDirectoryIfMissing)

import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (fromMaybe)
import Prelude hiding (read)

data Config = Config
  { authorize :: Txt -> IO Bool
  , validate :: File -> IO (Maybe Media)
  }

library :: Config -> Endpoints _ _ _ _
library config = Endpoints API.api msgs reqs
  where
    msgs = WS.none
    reqs = handleGetLibrary config
       <:> handleUpload config 
       <:> WS.none

handleGetLibrary :: Config -> RequestHandler API.GetLibrary
handleGetLibrary Config { authorize } = responding do
  un <- acquire
  authorized <- liftIO (authorize un)
  if authorized then do
    ml <- Sorcerer.read (LibraryStream un) 
    reply (Just (fromMaybe (Library []) ml))
  else do
    reply Nothing

handleUpload :: Config -> RequestHandler API.Upload
handleUpload Config { authorize, validate } = responding do
  file <- acquire
  now <- liftIO time
  mm <- liftIO (validate file)
  case mm of
    Nothing -> reply Nothing
    Just m -> do
      liftIO do
        createDirectoryIfMissing True (takeDirectory (fromTxt (path m)))
        writeByteTxt (fromTxt (path m)) (snd file)
        Sorcerer.write (LibraryStream (owner m)) (CreateMedia m)
      reply (Just m)
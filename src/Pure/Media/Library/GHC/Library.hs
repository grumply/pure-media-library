{-# language BlockArguments, NamedFieldPuns, PartialTypeSignatures, LambdaCase, ScopedTypeVariables #-}
module Pure.Media.Library.GHC.Library (Config(..),library) where

import Pure.Media.Library.API as API
import Pure.Media.Library.Data.Library hiding (library)
import Pure.Media.Library.Data.Media

import Pure.Data.Txt (Txt,FromTxt(..),ToTxt(..))
import Pure.Data.Time (time)
import Pure.WebSocket as WS
import Pure.ReadFile (writeByteTxt)
import Pure.Sorcerer as Sorcerer

import System.FilePath (takeDirectory,(</>))
import System.Directory (createDirectoryIfMissing,removeFile)

import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (fromMaybe)
import Prelude hiding (read)

import Debug.Trace

data Config = Config
  { root      :: Txt
  , authorize :: Txt -> IO Bool
  , validate  :: File -> IO (Maybe Media)
  }

library :: Config -> Endpoints _ _ _ _
library config = Endpoints API.api msgs reqs
  where
    msgs = WS.none
    reqs = handleGetLibrary config
       <:> handleUpload config 
       <:> handleDelete config
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
handleUpload Config { root, authorize, validate } = responding do
  file <- acquire
  liftIO (validate file) >>= \case
    Just m ->
      let fp = fromTxt root <> fromTxt (path m)
      in
        Sorcerer.transact (LibraryStream (owner m)) (CreateMedia m) >>= \case
          Update (l :: Library) -> do
            liftIO do 
              createDirectoryIfMissing True (takeDirectory fp)
              writeByteTxt fp (snd file)
            reply (Just m)
          _ -> 
            reply Nothing
    _ -> reply Nothing

handleDelete :: Config -> RequestHandler API.Delete
handleDelete Config { root, authorize, validate } = responding do
  media <- acquire
  authorized <- liftIO (authorize (owner media))
  if authorized then do
    Sorcerer.transact (LibraryStream (owner media)) (DeleteMedia media) >>= \case
      Update (l :: Library) -> do
        liftIO (removeFile (fromTxt root <> fromTxt (path media)))
        reply True
      _ -> 
        reply False
  else do
    reply False

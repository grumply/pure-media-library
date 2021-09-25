{-# language PatternSynonyms, LambdaCase, TypeApplications, PostfixOperators, BlockArguments, RecordWildCards, DataKinds, NamedFieldPuns, OverloadedStrings, TypeFamilies, RankNTypes, FlexibleContexts #-}
module Pure.Media.Library.Browser (Browser(..),Renderer(..)) where

import qualified Pure.Media.Library.API as API
import qualified Pure.Media.Library.Browser.Upload as Upload
import qualified Pure.Media.Library.Data.Media as Media
import qualified Pure.Media.Library.Data.Library as Library

import Pure.Control.Sync
import Pure.Data.Lifted (prevDef,prevProp)
import Pure.Elm.Component hiding (not,step,select)
import Pure.Intersection ( pattern RootMargin )
import qualified Pure.Stream as S
import Pure.WebSocket as WS

import Prelude hiding (max,reverse)
import Control.Monad (when)
import Control.Concurrent (newEmptyMVar,putMVar,takeMVar)
import qualified Data.List as List

data Renderer = Renderer
  { refresh :: IO ()
  , stream  :: S.Streamer Media.Media
  , form    :: View
  }

data Browser = Browser 
  { socket :: WebSocket
  , user   :: Txt
  , render :: Renderer -> View
  }

instance Component Browser where
  data Model Browser = Model
    { reload :: Bool }
    
  model = Model def
  
  data Msg Browser = Refresh

  upon = \case
    Refresh -> refreshBrowser

  view Browser { socket, user, render } Model { reload } = 
    (if reload then Tagged @True else Tagged @False)
      (render Renderer
        { refresh = command Refresh
        , stream = media 
        , form = form
        }
      )
    where
      media = def
        { S.producer = S.chunksOf 12 (S.unfolds Nothing getLibrary)
        , S.consumer = const []
        } 
        where
          getLibrary (Just l) = 
            case l of
              []       -> S.done
              (m:rest) -> S.more m (Just rest)

          getLibrary Nothing =
            sync (request API.api socket API.getLibrary user) >>= \case
              Just (Library.Library lib) -> getLibrary (Just (List.reverse lib))
              Nothing                    -> S.done


      form = run Upload.Upload {..}
        where 
          onUpload f =
            request API.api socket API.upload f $ \case
              -- Force the browser to reload the entire media stream. 
              -- Not especially efficient, but the browser cache does 
              -- a lot of the heavy lifting here.
              Just _success -> command Refresh 

              _failure      -> pure ()

refreshBrowser :: Update Browser
refreshBrowser _ mdl = pure mdl { reload = not (reload mdl) }
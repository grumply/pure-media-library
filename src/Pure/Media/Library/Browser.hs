{-# language PatternSynonyms, LambdaCase, TypeApplications, PostfixOperators, BlockArguments, RecordWildCards, DataKinds, NamedFieldPuns, OverloadedStrings, TypeFamilies, RankNTypes, FlexibleContexts, ScopedTypeVariables, ViewPatterns #-}
module Pure.Media.Library.Browser (Browser(..),Renderer(..)) where

import qualified Pure.Media.Library.API as API
import qualified Pure.Media.Library.Data.Media as Media
import qualified Pure.Media.Library.Data.Library as Library

import Pure.Sync
import Pure.Data.Lifted (prevDef,prevProp)
import Pure.Data.Lifted as Lifted (Node(Node),(..#))
import Pure.Elm.Component hiding (not,step,select)
import Pure.Intersection ( pattern RootMargin )
import Pure.ReadFile as ReadFile (ByteTxt,getFile )
import qualified Pure.Stream as S
import Pure.WebSocket as WS

import Prelude hiding (max,reverse)
import Control.Monad (when)
import Control.Concurrent (newEmptyMVar,putMVar,takeMVar)
import qualified Data.List as List
import Data.Typeable (Typeable)

data Renderer domain = Renderer
  { refresh :: IO ()
  , stream  :: S.Streamer (Media.Media domain)
  , input   :: View
  }

data Browser domain = Browser 
  { socket :: WebSocket
  , user   :: Txt
  , render :: Renderer domain -> View
  }

instance Typeable domain => Component (Browser domain) where
  data Model (Browser domain) = Model
    { reload :: Bool }
    
  model = Model def
  
  data Msg (Browser domain) = Refresh

  upon = \case
    Refresh -> refreshBrowser

  view Browser { socket, user, render } Model { reload } = 
    (if reload then Tagged @True else Tagged @False)
      (render Renderer
        { refresh = command (Refresh @domain)
        , stream = media 
        , input = input
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
            sync (request (API.api @domain) socket (API.getLibrary @domain) user) >>= \case
              Just (Library.Library lib) -> getLibrary (Just (List.reverse lib))
              Nothing                    -> S.done

      input = Input <| OnClickWith clickOptions clickHandler . OnChange select . Type "file" . Accept "image/*"
        where
          clickOptions = Options False True False True
          clickHandler ev = pure ()

          select ev = do
            evtObj ev ..# "target" >>= \case
              Just (Lifted.Node -> target) -> 
                ReadFile.getFile target >>= \case 
                  Just file -> onUpload file
                  failure   -> pure ()
              failure       -> pure ()

          onUpload f =
            request (API.api @domain) socket (API.upload @domain) f $ \case
              -- Force the browser to reload the entire media stream. 
              -- Not especially efficient, but the browser cache does 
              -- a lot of the heavy lifting here.
              Just _success -> command (Refresh @domain)

              _failure      -> pure ()

refreshBrowser :: Update (Browser domain)
refreshBrowser _ mdl = pure mdl { reload = not (reload mdl) }
{-# language TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}
module Pure.Media.Library.API where

import Pure.Media.Library.Data.Media (File,Media)
import Pure.Media.Library.Data.Library (Library)

import Pure.Data.Txt (Txt)
import Pure.WebSocket as WS

mkRequest "GetLibrary"
  [t|Txt -> Maybe Library|]

mkRequest "Upload" 
  [t|File -> Maybe Media|]

api = WS.api msgs reqs
  where
    msgs = WS.none
    reqs = getLibrary <:> upload <:> WS.none
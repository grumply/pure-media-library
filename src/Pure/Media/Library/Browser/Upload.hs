{-# language LambdaCase, ViewPatterns, OverloadedStrings #-}
module Pure.Media.Library.Browser.Upload (upload) where

import Pure.Media.Library.Data.Media (File)

import Pure.Elm
import Pure.Data.Lifted as Lifted ( Node(Node), (..#) )
import Pure.ReadFile as ReadFile ( ByteTxt, getFile )

upload :: (File -> IO ()) -> (View -> View) -> View
upload onSelect form = form input
  where
    handle evt =
      evtObj evt ..# "target" >>= \case
        Just (Lifted.Node -> target) -> 
          ReadFile.getFile target >>= \case 
            Just file -> onSelect file
            failure   -> pure ()
        failure       -> pure ()

    input = 
      Input <| OnClickWith clickOptions clickHandler . OnChange handle . Type "file" . Accept "image/*"
      where
        clickOptions = Options False True False True
        clickHandler ev = pure ()

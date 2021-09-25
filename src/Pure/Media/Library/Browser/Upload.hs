{-# language LambdaCase, ViewPatterns, OverloadedStrings, NamedFieldPuns, TypeFamilies, RankNTypes, FlexibleContexts #-}
module Pure.Media.Library.Browser.Upload (Upload(..)) where

import Pure.Media.Library.Data.Media (File)

import Pure.Elm.Component hiding (Select,select)
import Pure.Data.Lifted as Lifted (Node(Node),(..#))
import Pure.ReadFile as ReadFile (ByteTxt,getFile )

data Upload = Upload
  { onUpload :: File -> IO ()
  }

instance Component Upload where
  data Msg Upload = Select Evt

  upon = \case
    Select ev -> select ev

  view _ _ =
    Input <| OnClickWith clickOptions clickHandler . OnChange (command . Select) . Type "file" . Accept "image/*"
      where
        clickOptions = Options False True False True
        clickHandler ev = pure ()
        
select :: Evt -> Update Upload
select ev Upload { onUpload } mdl = do
  evtObj ev ..# "target" >>= \case
    Just (Lifted.Node -> target) -> 
      ReadFile.getFile target >>= \case 
        Just file -> onUpload file
        failure   -> pure ()
    failure       -> pure ()
  pure mdl

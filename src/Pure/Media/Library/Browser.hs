{-# language PatternSynonyms, LambdaCase, TypeApplications, PostfixOperators, BlockArguments, RecordWildCards, DataKinds, NamedFieldPuns, OverloadedStrings #-}
module Pure.Media.Library.Browser 
  (Browser(..),browser
  ,defaultBrowserStyles
  ,defaultExitStyles
  ,defaultLibraryStyles
  ,defaultMediaStyles
  ,defaultUploadStyles
  ) where

import qualified Pure.Media.Library.API as API
import qualified Pure.Media.Library.Browser.Upload as Upload
import qualified Pure.Media.Library.Data.Media as Media
import qualified Pure.Media.Library.Data.Library as Library

import Pure.Data.Lifted (prevDef,prevProp)
import Pure.Elm hiding (not,step,select)
import Pure.Intersection ( pattern RootMargin )
import qualified Pure.Stream as S
import Pure.WebSocket as WS

import Prelude hiding (max,reverse)
import Control.Monad (when)
import Control.Concurrent (newEmptyMVar,putMVar,takeMVar)
import qualified Data.List as List

data Model = Model
  { reload :: Bool
  }

data Browser = Browser 
  { socket   :: WebSocket
  , user     :: Txt
  , onSelect :: Maybe Media.Media -> IO ()
  , onClose  :: IO ()
  }

data Msg = Refresh

type Update = Browser -> Model -> IO Model

browser :: Browser -> View
browser = run (Applet [] [] [] (pure mdl) update view)
  where mdl = Model False

update :: Msg -> Update
update = \case
  Refresh  -> refresh

refresh :: Update
refresh _ mdl = pure mdl { reload = not (reload mdl) }

view :: Elm Msg => Browser -> Model -> View
view browser@Browser {..} Model {..} = tag $ 
  Div <| Themed @Browser . OnDoc "click" (const onClose) |>
    [ Div <||> -- relative centering context
      [ Div <| Themed @Library.Library |>
        [ Header <||>
          [ H1 <||> [ "Media Library" ]
          , Div <||>
            [ upload browser
            , Button <| Themed @Exit . OnClickWith intercept (const onClose) |> 
              [ "X" ]
            ]
          ]
        , library browser (onSelect . Just) onClose
        ]
      ]
    ]
  where
    -- This is a hack around pure-stream not being reactive. 
    -- I consider this a reasonable trade-off, for now.
    tag | reload    = Tagged @True
        | otherwise = Tagged @False

library :: Browser -> (Media.Media -> IO ()) -> IO () -> View 
library Browser { socket, user } onSelect onClose = S.stream def
  { S.producer = S.chunksOf 12 (S.unfolds Nothing getLibrary)
  , S.consumer = fmap (media onSelect onClose) . S.toList
  , S.children = [ S.stepper <| RootMargin (200px) ]
  } where

  getLibrary (Just l) = 
    case l of
      []       -> S.done
      (m:rest) -> S.more m (Just rest)

  getLibrary Nothing = do
    mv <- newEmptyMVar
    request API.api socket API.getLibrary user (putMVar mv)
    rsp <- takeMVar mv
    case rsp of
      Just (Library.Library lib) -> getLibrary (Just (List.reverse lib))
      Nothing                    -> S.done

media :: (Media.Media -> IO ()) -> IO () -> Media.Media -> View
media onSelect onClose m@Media.Media {..} = 
  Div <| Themed @Media.Media |> 
    -- the OnMouseDown handles the case that the image shrinks (css transform)
    -- and the mouse is no longer over the image so the click doesn't get picked up.
    [ Img <| Src (Media.path m) . OnMouseDown select . OnTouchStart select ]
  where
    select _ = do
      delay (Milliseconds 150 0)
      onClose
      onSelect m

upload :: Elm Msg => Browser -> View
upload Browser { socket } = Upload.upload handle form
  where 
    handle f =
      request API.api socket API.upload f $ \case
        Just _success -> command Refresh
        _failure      -> pure ()
        
    form input = let i = "file_input" in
      Div <| Themed @Upload |>
        [ Label <| For i |> 
          [ "Select File For Upload" ]
        , input <| Id i
        ]

data Upload
instance Theme Upload

defaultUploadStyles = do
  display =: inline-block

instance Theme Browser 

defaultBrowserStyles = do
  position         =: fixed
  top              =: 0
  right            =: 0
  bottom           =: 0
  left             =: 0
  z-index          =: 1001
  width            =: (100%)
  height           =: (100%)
  overflow         =: hidden
  
  child (tag Div) do
    width            =: (90%)
    height           =: (90%)
    margin-top       =: (5%)
    margin-left      =: (5%)

instance Theme Library.Library

defaultLibraryStyles = do
  display             =: flex
  height              =: (100%)
  flex-direction      =: column
  position            =: relative
  background-color    =: white
  border              =* [1px,solid,black]
  box-shadow          =* [0,0,20px,black]
  overflow            =: hidden
  
  child (tag Header) do
    padding           =: 16px
    display           =: flex
    flex-direction    =: row
    justify-content   =: space-between
    height            =: 40px
    background-color  =: hex 0xdfdfdf
    
    child (tag H1) do
      margin          =: 0
      
    child (tag Div) do
      margin          =: 0

  child (tag Div) do
    overflow-y        =: scroll
    height            =: (100%)
    
    child (tag Div) $ firstChild do
      display         =: flex
      align-items     =: center
      justify-content =: center
      flex-direction  =: row
      flex-wrap       =: wrap

instance Theme Media.Media 

defaultMediaStyles = do
  height            =: 2 inch
  width             =: 2 inch
  margin            =: 0.25 inch
  display           =: flex
  align-items       =: center
  justify-content   =: center
  background-color  =: gray
  box-shadow        =* [0,0,5px,black,inset]

  child (tag Img) do

    max-height =: 2 inch
    max-width  =: 2 inch
    border     =* [1px,solid,black]
    box-shadow =* [0,0,5px,black]
    transition =* ["all",0.1s,easein]
    transform  =* [perspective <> "(500px)",translate3d(0,0,0)]

    hover do
      box-shadow =* [0,0,15px,black]
      transition-"delay" =: 0.15s
      transform  =* [perspective <> "(500px)",translate3d(0,0,50px)]
    
    active do
      box-shadow =* [0,0,5px,black]
      transition-"delay" =: 0s
      transform  =* [perspective <> "(500px)",translate3d(0,0,(-50)px)]

data Exit
instance Theme Exit

defaultExitStyles = do
  border-radius    =: (50%)
  border           =* [1px,solid,darkgray]
  background-color =: gray


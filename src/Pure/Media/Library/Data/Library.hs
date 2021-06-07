{-# language DeriveAnyClass, DerivingStrategies, MultiParamTypeClasses, TypeFamilies, DeriveGeneric #-}
module Pure.Media.Library.Data.Library where

import Pure.Media.Library.Data.Media (Media)

import Pure.Data.Txt (Txt)
import Pure.Data.JSON (ToJSON,FromJSON)

import Sorcerer

import Data.Hashable (Hashable)

import Data.List as List (filter,length)
import GHC.Generics (Generic)

data Library = Library
  { library :: [Media]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data LibraryMsg
  = CreateMedia Media
  | DeleteMedia Media
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance Source LibraryMsg where
  data Stream LibraryMsg = LibraryStream Txt
    deriving stock Generic
    deriving anyclass Hashable

instance Aggregable LibraryMsg Library where
  update (CreateMedia m) Nothing =
    Update (Library [m])

  update (CreateMedia m) (Just l) =
    Update l { library = library l ++ [m] }

  update (DeleteMedia m) (Just ms) =
    let l' = List.filter (/= m) (library ms)
     in if List.length l' /= List.length (library ms) then
          Update ms { library = l' }
        else
          Ignore

  update _ _ =
    Ignore


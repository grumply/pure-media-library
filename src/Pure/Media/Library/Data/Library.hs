{-# language DeriveAnyClass, DerivingStrategies, MultiParamTypeClasses, TypeFamilies, DeriveGeneric #-}
module Pure.Media.Library.Data.Library where

import Pure.Media.Library.Data.Media (Media)

import Pure.Data.Txt (Txt)
import Pure.Data.JSON (ToJSON,FromJSON)

import Pure.Sorcerer

import Data.Hashable (Hashable)

import Data.List as List (filter,length,elem,notElem)
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

instance Streamable LibraryMsg where
  data Stream LibraryMsg = LibraryStream Txt
    deriving stock (Generic,Eq,Ord)
    deriving anyclass Hashable

instance Aggregable LibraryMsg Library where
  update (CreateMedia m) Nothing =
    Update (Library [m])

  update (CreateMedia m) (Just l) | List.notElem m (library l) =
    Update l { library = library l ++ [m] }

  update (DeleteMedia m) (Just l) | List.elem m (library l) = 
    Update l { library = List.filter (/= m) (library l) }

  update _ _ =
    Ignore


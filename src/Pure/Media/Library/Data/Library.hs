{-# language DeriveAnyClass, DerivingStrategies, MultiParamTypeClasses, TypeFamilies, DeriveGeneric #-}
module Pure.Media.Library.Data.Library where

import Pure.Media.Library.Data.Media (Media)

import Pure.Data.Txt (Txt)
import Pure.Data.JSON (ToJSON,FromJSON)

import Pure.Sorcerer

import Data.Hashable (Hashable)

import Data.List as List (filter,length,elem,notElem)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

data Library domain = Library
  { library :: [Media domain]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data LibraryMsg domain
  = CreateMedia (Media domain)
  | DeleteMedia (Media domain)
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance Typeable domain => Streamable (LibraryMsg domain) where
  data Stream (LibraryMsg domain) = LibraryStream Txt
    deriving stock (Generic,Eq,Ord)
    deriving anyclass Hashable

instance Typeable domain => Aggregable (LibraryMsg domain) (Library domain) where
  update (CreateMedia m) Nothing =
    Update (Library [m])

  update (CreateMedia m) (Just l) | List.notElem m (library l) =
    Update l { library = library l ++ [m] }

  update (DeleteMedia m) (Just l) | List.elem m (library l) = 
    Update l { library = List.filter (/= m) (library l) }

  update _ _ =
    Ignore


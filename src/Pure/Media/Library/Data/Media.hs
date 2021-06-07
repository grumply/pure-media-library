{-# language DeriveAnyClass, DerivingStrategies, DeriveGeneric, OverloadedStrings #-}
module Pure.Media.Library.Data.Media (Link,File,Media(..),media) where

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Time (Time)
import Pure.Data.Txt as Txt (Txt,toTxt,span,null,toLower,length,tail)
import Pure.ReadFile ( ByteTxt, unsafeByteTxtToTxt )

import Data.Hashable (hash)

import GHC.Generics (Generic)

type Link = Txt
type File = (Txt,ByteTxt)

data Media = Media
  { owner   :: Txt
  , created :: Time
  , hash    :: Txt
  , ext     :: Txt
  , path    :: Txt
  } deriving stock (Eq,Ord,Show,Generic)
    deriving anyclass (ToJSON,FromJSON)

media :: Int -> Txt -> Time -> File -> Maybe Media
media maxFileSizeInBytes un tm (nm,cnt)
  | let c = unsafeByteTxtToTxt cnt
  , Txt.null c || Txt.length c > maxFileSizeInBytes
  = Nothing

  | otherwise =
    let
      (_,ext) = Txt.span (/= '.') nm
      h = toTxt $ abs $ Data.Hashable.hash cnt
      p = "/media/" <> un <> "/" <> h <> ext
    in 
      if Txt.length ext > 1 then
        Just (Media un tm h (Txt.tail ext) p)
      else
        Nothing
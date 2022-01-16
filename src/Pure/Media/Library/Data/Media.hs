{-# language DeriveAnyClass, DerivingStrategies, DeriveGeneric, OverloadedStrings, PatternSynonyms #-}
module Pure.Media.Library.Data.Media (Link,File,Media(..),media) where

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Marker (markIO,Marker())
import Pure.Data.Time (Time,pattern Milliseconds)
import Pure.Data.Txt as Txt (Txt,toTxt,span,null,toLower,length,tail,splitOn)
import Pure.ReadFile ( ByteTxt, unsafeByteTxtToTxt )

import Data.Hashable (hash)
import Data.Maybe (fromMaybe)

import GHC.Generics (Generic)

type Link = Txt
type File = (Txt,ByteTxt)

data Media domain = Media
  { owner   :: Txt
  , created :: Time
  , path    :: Txt
  } deriving stock (Eq,Ord,Show,Generic)
    deriving anyclass (ToJSON,FromJSON)

media :: Int -> Txt -> Time -> File -> IO (Maybe (Media domain))
media maxFileSizeInBytes un tm (nm,cnt)
  | let c = unsafeByteTxtToTxt cnt
  , Txt.null c || Txt.length c > maxFileSizeInBytes
  = pure Nothing

  | otherwise = do
    m <- markIO
    let p = un <> "/" <> toTxt m
    pure $ 
      Just (Media un tm p)
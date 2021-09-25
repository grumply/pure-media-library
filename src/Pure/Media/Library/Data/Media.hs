{-# language DeriveAnyClass, DerivingStrategies, DeriveGeneric, OverloadedStrings, PatternSynonyms #-}
module Pure.Media.Library.Data.Media (Link,File,Media(..),media) where

import Pure.Data.JSON (ToJSON,FromJSON)
import Pure.Data.Time (Time,pattern Milliseconds)
import Pure.Data.Txt as Txt (Txt,toTxt,span,null,toLower,length,tail,splitOn)
import Pure.ReadFile ( ByteTxt, unsafeByteTxtToTxt )

import Data.Hashable (hash)
import Data.Maybe (fromMaybe)

import GHC.Generics (Generic)

type Link = Txt
type File = (Txt,ByteTxt)

data Media = Media
  { owner   :: Txt
  , created :: Time
  , hash    :: Txt
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
      lastMay [] = Nothing
      lastMay [x] = Just x
      lastMay (_ : xs) = lastMay xs
      ext = fromMaybe nm $ lastMay $ Txt.splitOn "." nm
      Milliseconds ms _ = tm
      h = toTxt $ abs $ Data.Hashable.hash (cnt,ms)
      p = un <> "/" <> h <> "." <> ext
    in 
      if Txt.length ext > 1 then
        Just (Media un tm h p)
      else
        Nothing
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad ( (>=>), (<=<) )
import           Control.Monad.Reader
import           Control.Monad.Catch

import           Data.Aeson
import           Data.Aeson.Types ( parse )
import qualified Data.ByteString.Lazy as B
import           Data.Maybe ( fromJust )
import           Data.Text
import           Data.Vector ( (!) )

import           Formatting

import           GHC.Generics

import           Network.Wreq

import           Types

newtype SteamKey = SteamKey { _skey :: String }
  deriving (Generic, Show)
makeClassy ''SteamKey

newtype LastFmKey = LastFmKey { _lfmkey :: String }
  deriving (Generic, Show)
makeClassy ''LastFmKey

data APIs = APIs { _lastFm :: LastFmKey, _steam :: SteamKey }
          deriving (Generic, Show)
makeClassy ''APIs

instance HasLastFmKey APIs where
  lastFmKey = lastFm . lastFmKey
instance HasSteamKey APIs where
  steamKey = steam . steamKey

instance FromJSON SteamKey
instance FromJSON LastFmKey
instance FromJSON APIs
instance ToJSON SteamKey
instance ToJSON LastFmKey
instance ToJSON APIs

newtype LFMAlbum = LFMAlbum { unwrapAlbum :: MediaInfo }
    deriving (Show)

instance FromJSON LFMAlbum where
    parseJSON (Object v) =
      LFMAlbum <$> (MediaInfo <$> v .: "url"
                              <*> (((.: "#text") . (! 2)) =<< (v .: "image")))

data SteamGame = SG { name,img_icon_url,img_logo_url :: String
                    , appid,playtime_2weeks,playtime_forever :: Int
                    }
                 deriving (Generic, Show)

getGameInfo :: SteamGame -> MediaInfo
getGameInfo SG{..} = MediaInfo { url = unpack fullUrl, artPath = unpack fullArtPath }
  where
    fullArtPath = sformat ("http://media.steampowered.com/steamcommunity/public/images/apps/"
                           % int % "/" % string % ".jpg")
                          appid
                          img_logo_url
    fullUrl = sformat ("https://store.steampowered.com/app/" % int) appid

instance FromJSON SteamGame

----------------------

main :: IO ()
main = do
    apiKeys <- fromJust . decode <$> B.readFile ".keys" :: IO APIs
    (albumResult, gameResult) <- concurrently (runReaderT fetchRecentAlbums
                                                          apiKeys)
                                              (runReaderT fetchRecentGames
                                                          apiKeys)
    case (albumResult, gameResult) of
        (Success albums, Success games) ->
            B.writeFile "stats.json" . encode $
                Stats { music = albums, games = games }
        _ -> printFailure albumResult >> printFailure gameResult
  where
    printFailure (Success _) =
        return ()
    printFailure (Error s) =
        print s

----------------------

fetchMediaInfo :: (FromJSON a, MonadIO m, MonadThrow m)
               => (a -> Result b)
               -> String
               -> m (Result b)
fetchMediaInfo extract =
   liftIO . get >=> asJSON >>^ view responseBody >>^ extract

fetchRecentGames :: (HasSteamKey r, MonadReader r m, MonadIO m, MonadThrow m)
                 => m (Result [MediaInfo])
fetchRecentGames = do
    key <- view skey
    fetchMediaInfo extract $
        "http://api.steampowered.com/IPlayerService/GetRecentlyPlayedGames/v0001/?key="
            ++ key ++ "&steamid=76561197973371420&count=6&format=json"
  where
    extract :: Object -> Result [MediaInfo]
    extract = parse $
        (.: "response") >=> (.: "games") >>^ fmap getGameInfo

fetchRecentAlbums :: (HasLastFmKey r, MonadReader r m, MonadIO m, MonadThrow m) => m (Result [MediaInfo])
fetchRecentAlbums = do
    key <- view lfmkey
    fetchMediaInfo extract $
        "http://ws.audioscrobbler.com/2.0/?method=user.gettopalbums&user=steell&period=7day&limit=6&api_key="
            ++ key ++ "&format=json"
  where
    extract :: Object -> Result [MediaInfo]
    extract = parse $
        ((.: "topalbums") >=> (.: "album")) >>^ fmap unwrapAlbum

(>>^) :: Monad m => (b -> m c) -> (c -> d) -> b -> m d
k >>^ fn = \b -> return . fn =<< k b

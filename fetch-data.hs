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
import           Data.List ( intercalate )
import           Data.Maybe ( fromJust )
import           Data.Text hiding ( intercalate )
import           Data.Time.Clock.POSIX ( getPOSIXTime )
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

newtype FoursquareKey = FoursquareKey { _fskey :: String }
  deriving (Generic, Show)
makeClassy ''FoursquareKey

data APIs = APIs { _lastFm :: LastFmKey, _steam :: SteamKey, _foursquare :: FoursquareKey }
          deriving (Generic, Show)
makeClassy ''APIs

instance HasLastFmKey APIs where
  lastFmKey = lastFm . lastFmKey
instance HasSteamKey APIs where
  steamKey = steam . steamKey
instance HasFoursquareKey APIs where
  foursquareKey = foursquare . foursquareKey

instance FromJSON SteamKey
instance FromJSON LastFmKey
instance FromJSON FoursquareKey
instance FromJSON APIs
instance ToJSON SteamKey
instance ToJSON LastFmKey
instance ToJSON FoursquareKey
instance ToJSON APIs

newtype LFMAlbum = LFMAlbum { unwrapAlbum :: MediaInfo }
    deriving (Show)

instance FromJSON LFMAlbum where
    parseJSON (Object v) =
      LFMAlbum <$> (MediaInfo <$> v .: "url"
                              <*> (((.: "#text") . (! 2)) =<< (v .: "image"))
                              <*> artistAndAlbum)
      where artistAndAlbum =
              formatAlbumName <$> v .: "name"
                              <*> ((v .: "artist") >>= (.: "name"))

            formatAlbumName :: String -> String -> String
            formatAlbumName album artist = artist ++ " - " ++ album

data SteamGame = SG { name,img_icon_url,img_logo_url :: String
                    , appid,playtime_2weeks,playtime_forever :: Int
                    }
                 deriving (Generic, Show)

getGameInfo :: SteamGame -> MediaInfo
getGameInfo SG{..} = MediaInfo { url = unpack fullUrl, artPath = unpack fullArtPath, mediaName = name }
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
    (albumResult, gameResult, placeResult) <- runConcurrently $
                                                  (,,) <$> Concurrently (runReaderT fetchRecentAlbums
                                                                                    apiKeys)
                                                       <*> Concurrently (runReaderT fetchRecentGames
                                                                                    apiKeys)
                                                       <*> Concurrently (runReaderT fetchRecentPlaces
                                                                                    apiKeys)
    case (albumResult, gameResult, placeResult) of
        (Success albums, Success games, Success places) ->
            B.writeFile "stats.json" . encode $
                Stats { music = albums, games = games, places = places }
        _ -> printFailure albumResult >> printFailure gameResult >>
            printFailure placeResult
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

----------------------

newtype FSPlace = FSPlace { unwrapPlace :: Place }
mkFSPlace lat lng desc = FSPlace $ Place lat lng desc

instance FromJSON FSPlace where
    parseJSON (Object v) = do
        venue <- v .: "venue"
        loc <- venue .: "location"
        mkFSPlace <$> (loc .: "lat")
                  <*> (loc .: "lng")
                  <*> (venue .: "categories"
                           >>= mapM (.: "name")
                           >>^ intercalate ", ")


fetchRecentPlaces :: (HasFoursquareKey r, MonadReader r m, MonadIO m, MonadThrow m)
                  => m (Result [Place])
fetchRecentPlaces = url >>= liftIO . get >>= asJSON >>^ view responseBody >>^
    extract
  where
    extract :: Object -> Result [Place]
    extract = parse $
      ((.: "response") >=> (.: "checkins") >=> (.: "items")) >>^ fmap unwrapPlace

    url = mkUrl <$> (show . (subtract weekSecs) . round <$> liftIO getPOSIXTime)
                <*> view fskey

    mkUrl :: String -> String -> String
    mkUrl time key = "https://api.foursquare.com/v2/users/self/checkins?"
        ++ intercalate "&" [ "oauth_token=" ++ key, "afterTimestamp=" ++ time, "v=20170202" ]

    weekSecs = 604800

----------------------

(>>^) :: Monad m => (b -> m c) -> (c -> d) -> b -> m d
k >>^ fn = \b -> return . fn =<< k b

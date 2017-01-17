--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import           Data.Monoid (mappend, (<>))
import           Hakyll
import           Data.Text
import           Data.Aeson
import           Data.ByteString.Lazy
import           Data.Maybe ( fromJust )

import           Types

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match (fromList [ "about.rst", "contact.markdown" ]) $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "posts/*.md" $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    create [ "archive.html" ] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx = listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives" `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            stats <- fromJust . decode <$> loadBody "stats.json"
            let indexCtx = listField "games"
                                     mediaCtx
                                     (mapM makeItem $ games stats) <>
                    listField "albums" mediaCtx (mapM makeItem $ music stats) <>
                    listField "posts" postCtx (return posts) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    match "stats.json" $ compile getResourceLBS

decodeStats :: ByteString -> Stats
decodeStats = fromJust . decode

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

mediaCtx =
          field "artPath" (return . artPath . itemBody) <>
          field "externalUrl" (return . url . itemBody)

(>>^) :: Monad m => (b -> m c) -> (c -> d) -> b -> m d
k >>^ fn = \b -> return . fn =<< k b

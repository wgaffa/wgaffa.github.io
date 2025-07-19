{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)

import System.Process
import Chronos
import Hakyll
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)

import Text.Pandoc.Class
import Text.Pandoc.Options
import Text.Pandoc.Readers

import Control.Monad

import Data.ByteString qualified as BS

yearIO :: IO String
yearIO = show . getYear . dateYear . datetimeDate . timeToDatetime <$> now

copyFilesList :: [Identifier]
copyFilesList = ["js/*", "icons/*", "images/*"]

copyRule :: Rules ()
copyRule =
    match (fromList copyFilesList) $ do
        route idRoute
        compile copyFileCompiler

cssRule :: Rules ()
cssRule = do
    match ("css/*" .&&. complement "css/tachyons*.css") $ do
        route idRoute
        compile compressCssCompiler

postsRule :: Rules ()
postsRule =
    match "posts/*" $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate
                    "templates/post.html"
                    postCtx
                >>= relativizeUrls

indexRule :: Rules ()
indexRule =
    create ["index.html"] $ do
        route idRoute
        year <- preprocess yearIO
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let
                indexCtx =
                    listField "posts" (constField "year" year <> postCtx) (pure posts)
                        <> constField "title" "Home"
                        <> constField "year" year

                bioCtx =
                    listField
                        "contacts"
                        (field "title" (pure . fst . itemBody) <> field "target" (pure . snd . itemBody))
                        (sequence [makeItem ("github", "https://www.github.com/wgaffa")])

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" (indexCtx <> postCtx)
                >>= loadAndApplyTemplate "templates/default.html" (indexCtx <> bioCtx <> postCtx)
                >>= relativizeUrls

syntaxHighlightRule :: Rules ()
syntaxHighlightRule =
    create ["css/syntax.css"] $ do
        route idRoute
        compile $ do
            makeItem $ styleToCss pandocCodeStyle

tachyonsRule :: Rules ()
tachyonsRule = do
    create ["css/tachyons.min.css"] $ do
        route idRoute
        compile $ do
            unsafeCompiler (callProcess "npm" ["run", "build:minify"])
            unsafeCompiler (BS.readFile "css/tachyons.min.css") >>= makeItem

templateRule :: Rules ()
templateRule = match "templates/*" $ compile templateBodyCompiler

main :: IO ()
main = do
    hakyll $
        sequence_
            [ cssRule
            , copyRule
            , postsRule
            , indexRule
            , syntaxHighlightRule
            , templateRule
            , tachyonsRule
            ]

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"
        <> metadataListField
        <> defaultContext

pandocCodeStyle :: Style
pandocCodeStyle = breezeDark

getMetadataListField ::
    (MonadMetadata m) => Identifier -> String -> m (Maybe [String])
getMetadataListField identifier key = do
    metadata <- getMetadata identifier
    pure $ lookupStringList key metadata

metadataListField :: Context a
metadataListField = Context $ \k _ i -> do
    values <- getMetadataListField (itemIdentifier i) k
    case values of
        Just vs -> do
            listItems <- mapM makeItem vs
            pure $ ListField (field "item" (pure . itemBody)) listItems
        Nothing -> noResult $ "Tried field " ++ k

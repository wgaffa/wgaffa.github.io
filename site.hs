{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)

import Chronos
import Hakyll
import System.Process
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)

import Text.Pandoc (Pandoc, Inline (..))
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Class
import Text.Pandoc.Options
import Text.Pandoc.Readers

import Control.Monad

import Data.ByteString qualified as BS

yearIO :: IO String
yearIO = show . getYear . dateYear . datetimeDate . timeToDatetime <$> now

copyFilesList :: [Identifier]
copyFilesList = ["css/tachyons.min.css"]

copyRule :: Rules ()
copyRule =
    match ("js/*" .||. fromList copyFilesList) $ do
        route idRoute
        compile copyFileCompiler

cssRule :: Rules ()
cssRule = do
    match ("css/*" .&&. complement "css/tachyons.min.css") $ do
        route idRoute
        compile compressCssCompiler

postsRule :: Rules ()
postsRule =
    match "posts/*" $ do
        route $ setExtension "html"
        compile $
            pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions addLinkClasses
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

indexRule :: Rules ()
indexRule =
    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let
                indexCtx =
                    listField "posts" postCtx (pure posts)
                        <> constField "title" "Home"

                bioCtx =
                    listField
                        "contacts"
                        (field "title" (pure . fst . itemBody) <> field "target" (pure . snd . itemBody))
                        (sequence [makeItem ("github", "https://www.github.com/wgaffa")])

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" (indexCtx <> postCtx)
                >>= loadAndApplyTemplate "templates/default.html" (indexCtx <> bioCtx <> postCtx)
                >>= relativizeUrls

addLinkClasses :: Pandoc -> Pandoc
addLinkClasses = walk go
  where
    go (Link attr inlines target) =
        let (ident, classes, attrs) = attr
            classes' = classes ++ ["link", "light-blue", "hover-blue"]
        in Link (ident, classes', attrs) inlines target
    go x = x

syntaxHighlightRule :: Rules ()
syntaxHighlightRule =
    create ["css/syntax.css"] $ do
        route idRoute
        compile $ do
            makeItem $ styleToCss pandocCodeStyle

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

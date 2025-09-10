{-# LANGUAGE OverloadedStrings #-}

import Data.List (intercalate)

import Data.Text (Text)

import Hakyll
import System.Process
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)

import Text.Pandoc (Inline (..), Pandoc)
import Text.Pandoc.Class
import Text.Pandoc.Options
import Text.Pandoc.Readers
import Text.Pandoc.Walk (walk)

import Formatting

import Control.Monad

import Data.ByteString qualified as BS

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

postsRule :: Tags -> Rules ()
postsRule tags =
    match "posts/**" $ do
        route $ setExtension "html"
        compile $ do
            postTags <- getTags =<< getUnderlying
            let
                ctx =
                    listField
                        "tags"
                        ( field "tag" (pure . itemBody)
                            <> field "tagurl" (pure . ('/' :) . toFilePath . tagsMakeId tags . itemBody)
                        )
                        (sequence . fmap makeItem $ postTags)
                        <> defaultContext

            pandocCompilerWithTransform
                defaultHakyllReaderOptions
                defaultHakyllWriterOptions
                addLinkClasses
                >>= loadAndApplyTemplate "templates/post.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

tagRule :: Tags -> Rules ()
tagRule tags = do
    tagsRules tags $ \tagStr tagsPattern -> do
        route $ idRoute
        compile $ do
            posts <- recentFirst =<< loadAll tagsPattern
            postItemTempl <- loadBody "templates/post-item.html"
            postList <- applyTemplateList postItemTempl postCtx posts

            let tagCtx =
                    constField "title" tagStr
                        <> constField "postlist" postList
                        <> constField "tag" tagStr
                        <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tags.html" tagCtx
                >>= loadAndApplyTemplate "templates/default.html" tagCtx
                >>= relativizeUrls

tagList :: Tags -> Rules ()
tagList tags =
    create ["tags.html"] $ do
        route idRoute
        compile $ do
            taglist <- renderTags produceTag joinTags tags
            makeItem taglist
                >>= loadAndApplyTemplate "templates/tag-list.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls
  where
    produceTag tag url count minCount maxCount = formatToString
        ("<a href='" % string % "' class='link dim br2 ph2 pv1 bg-dark-gray light-blue hover-blue'>"
            % string % " (" % int % ")</a>") url tag count
    joinTags = concatMap $ formatToString ("<li class='ma1'>" % string % "</li>")

indexRule :: Rules ()
indexRule =
    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/**"
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
            makeItem (styleToCss pandocCodeStyle) >>= makeItem . compressCss . itemBody

templateRule :: Rules ()
templateRule = match "templates/**" $ compile templateBodyCompiler

main :: IO ()
main = do
    hakyll $ do
        tags <- buildTags "posts/**" $ fromCapture "tags/*.html"
        sequence_
            [ cssRule
            , copyRule
            , postsRule tags
            , indexRule
            , syntaxHighlightRule
            , tagRule tags
            , tagList tags
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
            listItems <- traverse makeItem vs
            pure $ ListField (field "item" (pure . itemBody)) listItems
        Nothing -> noResult $ "Tried field " ++ k

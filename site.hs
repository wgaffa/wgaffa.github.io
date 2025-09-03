{-# LANGUAGE OverloadedStrings #-}

import Data.List (intercalate)

import Data.Text (Text)

import Chronos
import Hakyll
import System.Process
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)

import Text.Pandoc (Inline (..), Pandoc)
import Text.Pandoc.Class
import Text.Pandoc.Options
import Text.Pandoc.Readers
import Text.Pandoc.Walk (walk)

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
        compile $ do
            tags <- getTags =<< getUnderlying
            tagList <- buildTags "posts/**" $ fromCapture "/tags/*.html"
            let
                ctx =
                    listField
                        "tags"
                        ( field "tag" (pure . itemBody)
                            <> field "tagurl" (pure . toFilePath . tagsMakeId tagList . itemBody)
                        )
                        (sequence . fmap makeItem $ tags)
                        <> defaultContext

            pandocCompilerWithTransform
                defaultHakyllReaderOptions
                defaultHakyllWriterOptions
                addLinkClasses
                >>= loadAndApplyTemplate "templates/post.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

tagRule :: Rules ()
tagRule = do
    tags <- buildTags "posts/*" $ fromCapture "tags/*.html"
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

tagList :: Rules ()
tagList =
    create ["tags.html"] $ do
        route idRoute
        tags <- buildTags "posts/*" $ fromCapture "tags/*.html"
        compile $ do
            taglist <- renderTags produceTag joinTags tags
            makeItem taglist
                >>= loadAndApplyTemplate "templates/tag-list.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls
  where
    produceTag tag url count minCount maxCount =
        "<a href='"
            ++ url
            ++ "' class='link dim br2 ph2 pv1 bg-dark-gray light-blue hover-blue'>"
            ++ tag
            ++ " ("
            ++ show count
            ++ ")"
            ++ "</a>"
    joinTags = concatMap (\tag -> "<li class='ma1'>" ++ tag ++ "</li>")

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
            , tagRule
            , tagList
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

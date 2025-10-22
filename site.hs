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

            myCompiler
                >>= loadAndApplyTemplate "templates/post.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

myCompiler :: Compiler (Item String)
myCompiler =
    pandocCompilerWithTransform
        ( defaultHakyllReaderOptions
            { readerExtensions =
                enableExtension Ext_fenced_code_attributes $
                    enableExtension Ext_raw_html (readerExtensions defaultHakyllReaderOptions)
            }
        )
        ( defaultHakyllWriterOptions
            { writerExtensions =
                enableExtension Ext_fenced_code_attributes $
                    enableExtension Ext_raw_html (writerExtensions defaultHakyllWriterOptions)
            }
        )
        addLinkClasses

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

tagList :: Identifier -> Tags -> Rules ()
tagList ident tags =
    create [ident] $ do
        route idRoute
        compile $ do
            taglist <- renderTags produceTag joinTags tags
            makeItem taglist
                >>= loadAndApplyTemplate "templates/tag-list.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls
  where
    produceTag tag url count minCount maxCount =
        formatToString
            ( "<a href='"
                % string
                % "' class='link dim br2 ph2 pv1 bg-dark-gray light-blue hover-blue'>"
                % string
                % " ("
                % int
                % ")</a>"
            )
            url
            tag
            count
    joinTags = concatMap $ formatToString ("<li class='ma1'>" % string % "</li>")

index :: Tags -> Rules ()
index tags = do
    create ["index.html"] $ do
        route idRoute
        compile $ do
            let
                applyTemplates initItem =
                    foldM
                        ( \acc x ->
                            loadAndApplyTemplate
                                (fromFilePath $ "templates/cards/" ++ x ++ ".html")
                                ( postCtx
                                    <> constField "name" (formatToString (titlecased string) x)
                                    <> constField "tagurl" (toFilePath $ tagsMakeId tags x)
                                )
                                acc
                        )
                        initItem
                        . fmap fst
                        . tagsMap
                        $ tags

            makeItem ""
                >>= applyTemplates
                >>= loadAndApplyTemplate "templates/index.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls
  where

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
        cats <- buildCategories "posts/**" $ fromCapture "categories/*.html"
        sequence_
            [ cssRule
            , copyRule
            , postsRule tags
            , syntaxHighlightRule
            , tagRule tags
            , tagList "tags.html" tags
            , tagRule cats
            , tagList "categories.html" cats
            , index cats
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

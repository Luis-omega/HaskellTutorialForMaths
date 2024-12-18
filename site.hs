--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import Data.Monoid (mappend)
import qualified Data.Text as Te
import Debug.Trace
import Hakyll
import Text.Pandoc.Definition
import Text.Pandoc.Extensions
import Text.Pandoc.Highlighting (espresso, pygments, styleToCss)
import qualified Text.Pandoc.Options as T
import Text.Pandoc.Walk

config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "docs",
      previewHost = "0.0.0.0"
    }

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match (fromList ["about.rst", "contact.markdown"]) $ do
    route $ setExtension "html"
    compile $
      pandocCompilerWith defaultHakyllReaderOptions pandocWriteOptions
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      getResourceBody
        >>= renderPandocWith pandocReaderOptions pandocWriteOptions
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  match "AdventOfCode/Blogs/*" $ do
    route $ setExtension "html"
    compile $
      getResourceBody
        >>= renderPandocWith pandocReaderOptions pandocWriteOptions
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  -- match "posts/*" $ version "haskell" $ do
  --   route $ setExtension "lhs"
  --   compile getResourceBody

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll regularPost
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      postsRegular <- loadAll regularPost
      postsAdventOfCode <- loadAll adventOfCodePosts
      let allPost = postsRegular ++ postsAdventOfCode
      posts <- recentFirst allPost
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Home"
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

  create ["css/syntax.css"] $ do
    route idRoute
    compile $ do
      makeItem $ styleToCss pandocCodeStyle

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext

regularPost = "posts/*" .&&. hasNoVersion

adventOfCodePosts = "AdventOfCode/Blogs/*" .&&. hasNoVersion

haskellPost = "posts/*" .&&. hasVersion "haskell"

pandocCodeStyle = espresso

pandocWriteOptions :: T.WriterOptions
pandocWriteOptions =
  defaultHakyllWriterOptions
    { T.writerHighlightStyle = Just pandocCodeStyle,
      T.writerHTMLMathMethod = T.MathJax ""
    }

pandocReaderOptions :: T.ReaderOptions
pandocReaderOptions = defaultHakyllReaderOptions {T.readerExtensions = newExtensions2}
  where
    newExtensions =
      T.enableExtension Ext_literate_haskell $
        T.readerExtensions defaultHakyllReaderOptions
    newExtensions2 = T.enableExtension Ext_tex_math_dollars newExtensions

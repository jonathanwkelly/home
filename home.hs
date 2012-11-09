{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow    ((***), (>>>), arr)
import Control.Category (id)
import Control.Monad    (forM_)
import Data.Char        (isLetter)
import Data.Monoid      (mconcat, mempty)
import System.FilePath  (takeFileName)

import Prelude hiding (id)
import Hakyll
import Hakyll.Web.Feed

main = hakyll $ do
  -- Render index page
  match  "index.html" $ route idRoute
  create "index.html" $ constA mempty
    >>> arr (setField "title" "Index")
    >>> requireAllA "posts/*" (id *** arr (take 10 . reverse . chronological) >>> postList)
    >>> applyTemplateCompiler "pages/index.html"
    >>> applyTemplateCompiler "templates/default.html"

  -- Render posts list
  match  "posts.html" $ route idRoute
  create "posts.html" $ constA mempty
    >>> arr (setField "title" "All posts")
    >>> requireAllA "posts/*" postList
    >>> applyTemplateCompiler "templates/postlist.html"
    >>> applyTemplateCompiler "templates/default.html"

  match "rss.xml" $ route idRoute
  create "rss.xml" $ requireAll_ "posts/*" >>> renderRss feedConfiguration

  -- Render all blog posts
  match "posts/*" $ do
    route   $ setExtension "html" `composeRoutes` postRoute
    compile $ pageCompiler >>> postCompiler

  -- Tags
  create "tags" $ requireAll "posts/*" (const readTags :: t -> [Page String] -> Tags String)

  -- Add a tag list compiler for every tag
  match "tags/*" $ route $ setExtension ".html"
  metaCompile $ require_ "tags"
    >>> arr tagsMap
    >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

  forM_ ["templates/*", "pages/*"] . flip match $ compile templateCompiler

  staticFiles

postList = setFieldA "posts" $
  arr (reverse . chronological)
  >>> require "templates/postitem.html" (flip (map . applyTemplate))
  >>> arr mconcat
  >>> arr pageBody

-- Turn "posts/2011-11-21-post.html" into "post.html"
postRoute = customRoute (dropWhile (not . isLetter) . takeFileName . toFilePath)

-- Turn "pages/foo.html" into "foo.html"
topRoute = customRoute (takeFileName . toFilePath)

postCompiler = arr (copyField "date" "isoDate")
  >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
  >>> arr (renderDateField "isoDate" "%Y-%m-%d" "Date unknown")
  >>> renderTagsField "prettytags" (fromCapture "tags/*")
  >>> applyTemplateCompiler "templates/postmeta.html"
  >>> applyTemplateCompiler "templates/post.html"
  >>> applyTemplateCompiler "templates/default.html"

makeTagList tag posts = constA (mempty, posts)
  >>> postList >>> arr (setField "title" ("Posts tagged with " ++ tag))
  >>> applyTemplateCompiler "templates/postlist.html"
  >>> applyTemplateCompiler "templates/default.html"

tagIdentifier = fromCapture "tags/*"

-- All static assetts
staticFiles = do
  match "css/*" $ route idRoute >> compile compressCssCompiler
  forM_ ["fonts/**", "img/**", "js/**", "files/**"] . flip match $ route idRoute >> compile copyFileCompiler

feedConfiguration = FeedConfiguration
  { feedTitle       = "Jonas Westerlund’s blog"
  , feedDescription = "Personal blog of Jonas Westerlund"
  , feedAuthorName  = "Jonas Westerlund"
  , feedAuthorEmail = "jonas.westerlund@icloud.com"
  , feedRoot        = "http://jonaswesterlund.se"
  }

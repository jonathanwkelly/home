{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM, forM_)
import Data.Char (isLetter)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>), mconcat)
import Hakyll
import System.FilePath (takeFileName)

main = hakyllWith config $ do
  match ("fonts/**" .||. "img/**" .||. "files/**") $
    route idRoute >> compile copyFileCompiler

  forM_ ["css/*", "js/*", "templates/*", "pages/*"] . flip match $ compile templateCompiler

  tags <- buildTags "posts/*" $ fromCapture "tag/*.html"

  match "posts/*" $ do
    route   $ setExtension ".html" `composeRoutes` topRoute
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html" (postContext tags)
      >>= loadAndApplyTemplate "templates/default.html" defaultContext

  create ["posts.html"] $ do
    route idRoute
    compile $ do
      list <- postList id tags "posts/*"
      let postListContext = constField "title" "All posts" <> constField "posts" list <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/postlist.html" postListContext
        >>= loadAndApplyTemplate "templates/default.html"  postListContext

  tagsRules tags $ \tag pattern -> do
    let title = "Posts about " <> tag
    route idRoute
    compile $ do
      list <- postList id tags pattern
      let tagListContext = constField "title" title <> constField "posts" list <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/postlist.html" tagListContext
        >>= loadAndApplyTemplate "templates/default.html"  tagListContext

  create ["index.html"] $ do
    route topRoute
    compile $ do
      list <- postList (take 3) tags "posts/*"
      makeItem list
      let indexContext = constField "title" "Index" <> constField "posts" list <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "pages/index.html" indexContext
        >>= loadAndApplyTemplate "templates/default.html" indexContext

  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
      renderRss feedConfiguration feedContext posts

-- postList :: (Typeable a, Binary a) => ([Item a] -> [Item String]) -> Tags -> Pattern -> Compiler String
postList f tags pattern = do
  postItemTpl <- loadBody "templates/postitem.html"
  posts       <- fmap f . recentFirst =<< loadAll pattern
  applyTemplateList postItemTpl (postContext tags) posts

postContext tags = mconcat
  [ modificationTimeField "mtime" "%U"
  , dateField "date" "%B %e, %Y"
  , dateField "isoDate" "%Y-%m-%d"
  , tagsFieldWith' getTags "tags" tags
  , defaultContext
  ]

feedContext = mconcat
  [ bodyField "description"
  , dateField "date" "%B %e, %Y"
  , defaultContext
  ]

-- Turn "pages/foo.html" into "foo.html"
topRoute = customRoute (takeFileName . toFilePath)

tagsFieldWith' :: (Identifier -> Compiler [String]) -> String -> Tags -> Context a                          -- ^ Resulting context
tagsFieldWith' getTags' key tags = field key $ \item -> do
  tags' <- getTags' $ itemIdentifier item
  links <- forM tags' $ \tag -> do
    route' <- getRoute $ tagsMakeId tags tag
    return $ renderLink tag route'
  return . mconcat . catMaybes $ links
  where
    renderLink _   Nothing         = Nothing
    renderLink tag (Just filePath) = Just $
      "<li><a href=\"" ++ toUrl filePath ++ "\">" ++ tag ++ "</a></li>"

config = defaultConfiguration
  { deployCommand = "./minify.sh; rsync --checksum -avz \
                    \_site/* jonas@192.168.11.1:/usr/local/www/jonas/jonaswesterlund.se/"
  }

feedConfiguration = FeedConfiguration
  { feedTitle       = "Jonas Westerlund’s blog"
  , feedDescription = "Personal blog of Jonas Westerlund"
  , feedAuthorName  = "Jonas Westerlund"
  , feedAuthorEmail = "jonas.westerlund@icloud.com"
  , feedRoot        = "https://jonaswesterlund.se"
  }

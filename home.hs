{-# LANGUAGE ExplicitForAll, OverloadedStrings, UnicodeSyntax #-}

import Control.Monad (forM_)
import Control.Monad.Unicode ((≫), (≫=), (=≪))
import Data.Char (isLetter)
import Data.List.Unicode ((⧺))
import Data.Monoid (mconcat)
import Data.Monoid.Unicode ((⊕))
import Hakyll
import Prelude.Unicode ((∘))
import System.FilePath (takeFileName)

main ∷ IO ()
main = hakyllWith config $ do
  match ("fonts/**" .||. "img/**" .||. "js/**" .||. "files/**") $
    route idRoute ≫ compile copyFileCompiler

  match "css/**" $ route idRoute ≫ compile compressCssCompiler

  tags ← buildTags "posts/*" $ fromCapture "tag/*.html"

  match "posts/*" $ do
    route   $ setExtension ".html" `composeRoutes` postRoute
    compile $ pandocCompiler
      ≫= saveSnapshot "content"
      ≫= loadAndApplyTemplate "templates/postmeta.html" (postContext tags)
      ≫= loadAndApplyTemplate "templates/post.html" (postContext tags)
      ≫= loadAndApplyTemplate "templates/default.html" (postContext tags)
      ≫= relativizeUrls

  create ["posts.html"] $ do
    route idRoute
    compile $ do
      list ← postList tags "posts/*"
      makeItem ""
        ≫= loadAndApplyTemplate "templates/postlist.html"
          (constField "title" "Posts" ⊕ constField "posts" list ⊕ defaultContext)
        ≫= loadAndApplyTemplate "templates/default.html" defaultContext
        ≫= relativizeUrls

  -- Post tags
  tagsRules tags $ \ tag pattern → do
    let title = "Posts tagged with " ⧺ tag
    route idRoute
    compile $ do
      list ← postList tags pattern
      makeItem ""
        ≫= loadAndApplyTemplate "templates/postlist.html"
            (constField "title" title ⊕ constField "posts" list ⊕ defaultContext)
        ≫= loadAndApplyTemplate "templates/default.html" defaultContext
        ≫= relativizeUrls

  create ["index.html"] $ do
    route topRoute
    compile $ do
      list ← postList tags "posts/*"
      makeItem list
      let indexContext = constField "posts" list ⊕ defaultContext
      makeItem ""
        ≫= loadAndApplyTemplate "pages/index.html" indexContext
        ≫= loadAndApplyTemplate "templates/default.html" indexContext
        ≫= relativizeUrls

  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      posts ← recentFirst =≪ loadAllSnapshots "posts/*" "content"
      renderRss feedConfiguration feedContext posts

  forM_ ["templates/*", "pages/*"] $ flip match $ compile templateCompiler

postList ∷ Tags → Pattern → Compiler String
postList tags pattern = do
    postItemTpl ← loadBody "templates/postitem.html"
    posts       ← recentFirst =≪ loadAll pattern
    applyTemplateList postItemTpl (postContext tags) posts

postContext ∷ Tags → Context String
postContext tags = mconcat
  [ modificationTimeField "mtime" "%U"
  , dateField "date" "%B %e, %Y"
  , dateField "isoDate" "%Y-%m-%d"
  , tagsField "tags" tags
  , defaultContext
  ]

feedContext ∷ Context String
feedContext = mconcat
  [ bodyField "description"
  , dateField "date" "%B %e, %Y"
  , defaultContext
  ]

-- Turn "posts/2011-11-21-post.html" into "post.html"
postRoute ∷ Routes
postRoute = customRoute $ dropWhile (not ∘ isLetter) ∘ takeFileName ∘ toFilePath

-- Turn "pages/foo.html" into "foo.html"
topRoute ∷ Routes
topRoute = customRoute (takeFileName ∘ toFilePath)

config ∷ Configuration
config = defaultConfiguration
  { deployCommand = "rsync --checksum -avz \
                    \_site/* jonas@192.168.11.1:/usr/local/www/jonas/jonaswesterlund.se/"
  }

feedConfiguration ∷ FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle       = "Jonas Westerlund’s blog"
  , feedDescription = "Personal blog of Jonas Westerlund"
  , feedAuthorName  = "Jonas Westerlund"
  , feedAuthorEmail = "jonas.westerlund@icloud.com"
  , feedRoot        = "http://jonaswesterlund.se"
  }

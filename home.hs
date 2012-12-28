{-# LANGUAGE ExplicitForAll, OverloadedStrings, UnicodeSyntax #-}

import Control.Arrow (arr)
import Control.Arrow.Unicode ((⁂), (⋙))
import Control.Category (id)
import Control.Monad (forM_)
import Control.Monad.Unicode ((≫))
import Data.Char (isLetter)
import Data.List.Unicode ((⧺))
import Data.Monoid (mconcat)
import Data.Monoid.Unicode ((∅))
import Hakyll
import Prelude (($), IO, String, const, dropWhile, flip, map, not, reverse, take)
import Prelude.Unicode ((∘))
import System.FilePath (takeFileName)

main ∷ IO ()
main = hakyll $ do
  match "index.html" $ route idRoute
  match "posts.html" $ route idRoute
  match "rss.xml"    $ route idRoute
  match "tags/*"     ∘ route $ setExtension "html"

  _ ← match "posts/*" $ do
    route   $ setExtension "html" `composeRoutes` postRoute
    compile $ pageCompiler ⋙ postCompiler

  _ ← create "index.html" $ constA (∅)
    ⋙ arr (setField "title" "Index")
    ⋙ requireAllA "posts/*" (id ⁂ arr (take 10 ∘ reverse ∘ chronological) ⋙ postList)
    ⋙ applyTemplateCompiler "pages/index.html"
    ⋙ applyTemplateCompiler "templates/default.html"

  _ ← create "posts.html" $ constA (∅)
    ⋙ arr (setField "title" "All posts")
    ⋙ requireAllA "posts/*" postList
    ⋙ applyTemplateCompiler "templates/postlist.html"
    ⋙ applyTemplateCompiler "templates/default.html"

  _ ← create "tags" $ requireAll "posts/*" (const readTags ∷ t → [Page String] → Tags String)

  metaCompile $ require_ "tags"
    ⋙ arr tagsMap
    ⋙ arr (map makeList)

  forM_ ["templates/*", "pages/*"] ∘ flip match $ compile templateCompiler

  _ ← create "rss.xml" $ requireAll_ "posts/*" ⋙ renderRss feedConfiguration
  _ ← match "css/**" $ route idRoute ≫ compile compressCssCompiler
  forM_ ["fonts/**", "img/**", "js/**", "files/**"] ∘ flip match $ route idRoute ≫ compile copyFileCompiler

makeList ∷ ∀ α β. (String, [Page String]) → (Identifier α, Compiler β (Page String))
makeList (t, p) = (tagIdentifier t, makeTagList t p)

postList ∷ ∀ α. Compiler (Page α, [Page String]) (Page α)
postList = setFieldA "posts" $
  arr (reverse ∘ chronological)
  ⋙ require "templates/postitem.html" (flip (map ∘ applyTemplate))
  ⋙ arr mconcat
  ⋙ arr pageBody

-- Turn "posts/2011-11-21-post.html" into "post.html"
postRoute ∷ Routes
postRoute = customRoute $ dropWhile (not ∘ isLetter) ∘ takeFileName ∘ toFilePath

-- Turn "pages/foo.html" into "foo.html"
topRoute ∷ Routes
topRoute = customRoute (takeFileName ∘ toFilePath)

postCompiler ∷ Compiler (Page String) (Page String)
postCompiler = arr (copyField "date" "isoDate")
  ⋙ arr (renderDateField "date" "%B %e, %Y" "Date unknown")
  ⋙ arr (renderDateField "isoDate" "%Y-%m-%d" "Date unknown")
  ⋙ renderTagsField "prettytags" (fromCapture "tags/*")
  ⋙ applyTemplateCompiler "templates/postmeta.html"
  ⋙ applyTemplateCompiler "templates/post.html"
  ⋙ applyTemplateCompiler "templates/default.html"

makeTagList ∷ ∀ α. String → [Page String] → Compiler α (Page String)
makeTagList tag posts = constA ((∅), posts)
  ⋙ postList ⋙ arr (setField "title" ("Posts tagged with " ⧺ tag))
  ⋙ applyTemplateCompiler "templates/postlist.html"
  ⋙ applyTemplateCompiler "templates/default.html"

tagIdentifier ∷ ∀ α. String → Identifier α
tagIdentifier = fromCapture "tags/*"

feedConfiguration ∷ FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle       = "Jonas Westerlund’s blog"
  , feedDescription = "Personal blog of Jonas Westerlund"
  , feedAuthorName  = "Jonas Westerlund"
  , feedAuthorEmail = "jonas.westerlund@icloud.com"
  , feedRoot        = "http://jonaswesterlund.se"
  }

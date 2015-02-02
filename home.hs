{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (filterM, forM, forM_, liftM)
import Data.Hashable
import Data.List (intersperse, sortBy)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>), mconcat)
import Data.Ord (comparing)
import Hakyll
import Numeric (showHex)
import System.FilePath (takeFileName)
import System.Locale (defaultTimeLocale)

main = hakyllWith config $ do
  match ("img/**" .||. "files/**") $ route idRoute >> compile copyFileCompiler

  forM_ ["css/*", "js/*", "templates/*", "pages/*"] . flip match $ compile templateCompiler

  tags <- buildTags "posts/*" $ fromCapture "tag/*.html"

{-
  tagsRules tags $ \tag pattern -> do
    let title = "Posts about " <> tag
    route idRoute
    compile $ do
      list <- postList id tags pattern
      let tagListContext = constField "title" title <> constField "posts" list <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/postlist.html" tagListContext
        >>= loadAndApplyTemplate "templates/default.html"  tagListContext
-}

  match "posts/*" $ do
    route   $ setExtension ".html" `composeRoutes` topRoute
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html" (postContext tags)
      >>= loadAndApplyTemplate "templates/default.html" (postContext tags)

  let indexContext = constField "title" "Jonas Westerlund" <> (postContext tags) <> defaultContext
  postIndex "posts/*" 4 indexContext

{-  
  create ["archive.html"] $ do
    route topRoute
    compile $ do
      list <- postList (take 4) tags "posts/*"
      makeItem list
      let indexContext = constField "title" "Index" <> constField "posts" list <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "pages/archive.html" indexContext
        >>= loadAndApplyTemplate "templates/default.html" indexContext
-}

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

tagsFieldWith' :: (Identifier -> Compiler [String]) -> String -> Tags -> Context a
tagsFieldWith' getTags' key tags = field key $ \item -> do
  tags' <- getTags' $ itemIdentifier item
  links <- forM tags' $ \tag -> do
    route' <- getRoute $ tagsMakeId tags tag
    return $ renderLink tag route'
  return . mconcat . intersperse ", " . catMaybes $ links
  where
    renderLink _   Nothing         = Nothing
    renderLink tag (Just filePath) = Just $
      "<a href=\"" ++ toUrl filePath ++ "\">" ++ tag ++ "</a>"

tagColor :: String -> String
tagColor tag = '#' : showHex (hash tag `mod` 16777215) ""

paginatePosts :: Pattern -> Int -> (Maybe Int -> Int -> Maybe Int -> [Identifier] -> Rules ()) -> Rules ()
paginatePosts pattern n rules = do
  let sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
      sortByM f xs   = liftM (map fst . sortBy (comparing snd)) $
                       mapM (\x -> liftM (\y -> (x, y)) (f x)) xs
      chronological' = sortByM $ getItemUTC defaultTimeLocale
      recentFirst'   = liftM reverse . chronological'
  ids <- recentFirst' =<< filterDrafts =<< getMatches pattern
  let chunks     = chunksOf n ids
      indexPages = zip chunks [1..]
      lastIndex  = length indexPages
  forM_ indexPages $ \(ps, i) -> let newer = if i > 1         then Just (i - 1) else Nothing
                                     older = if i < lastIndex then Just (i + 1) else Nothing
                                 in  rules older i newer ps

postIndex :: Pattern -> Int -> Context String -> Rules ()
postIndex pattern nPages context =
  paginatePosts pattern nPages $ \older current newer ids -> do
      let olderUrl   = fmap indexPageUrl older
          newerUrl   = fmap indexPageUrl newer
          identifier = fromFilePath $ drop 1 $ indexPageUrl current
      create [identifier] $ do
        route idRoute
        compile $ do
          posts <- forM ids $ \postId -> loadSnapshot postId "content"
          let indexCtx = postsField (return posts) <>
                postIndexContext olderUrl newerUrl <>
                context
        
          makeItem (indexPageUrl current)
            >>= loadAndApplyTemplate "templates/postlist.html" indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = (take n xs) : chunksOf n (drop n xs)

postIndexContext :: Maybe FilePath -> Maybe FilePath -> Context String
postIndexContext prev next = 
  mconcat $ catMaybes [fmap (constField "previous") prev, fmap (constField "next") next]

indexPageUrl :: Int -> FilePath
indexPageUrl 1 = "/index.html"
indexPageUrl i = "/page-" ++ show i ++ ".html"

postsField :: Compiler [Item String] -> Context a
postsField posts = listField "posts"
  (teaserField "teaser" "content" <> defaultContext)
  posts

isNotDraft :: MonadMetadata m => Identifier -> m Bool
isNotDraft identifier = liftM (/= Just "true") (getMetadataField identifier "draft")

filterDrafts :: MonadMetadata m => [Identifier] -> m [Identifier]
filterDrafts  = filterM isNotDraft

config = defaultConfiguration
  { deployCommand = "rsync --checksum -avz \
                    \_site/* jonas@jonaswesterlund.se:/usr/local/www/jonas/jonaswesterlund.se/"
  }

feedConfiguration = FeedConfiguration
  { feedTitle       = "Jonas Westerlund’s blog"
  , feedDescription = "Personal blog of Jonas Westerlund"
  , feedAuthorName  = "Jonas Westerlund"
  , feedAuthorEmail = "jonas.westerlund@icloud.com"
  , feedRoot        = "https://jonaswesterlund.se"
  }

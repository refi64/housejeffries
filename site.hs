--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.List (isInfixOf)
import Data.Monoid ((<>))
import Hakyll
import qualified System.FilePath as Native
import System.FilePath.Posix
  ( (</>)
  , replaceExtension
  , splitFileName
  , takeBaseName
  , takeDirectory
  )

--------------------------------------------------------------------------------
-- | Don't ignore any files (e.g. dotfiles like .htaccess).
myConfig :: Configuration
myConfig = defaultConfiguration {ignoreFile = const False}

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith myConfig $ do
  match "resources/*" $ do
    route dropFirstDir
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "templates/*" $ compile templateCompiler

  match "_content/index.md" $ do
    route $ setExtension "html" `composeRoutes` dropFirstDir
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/base.html" defaultContext
      >>= relativizeUrls

  match "_content/articles/*" $ do
    route $ niceRoute `composeRoutes` dropFirstDir
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/page.html" postCtx
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/base.html" postCtx
      >>= relativizeUrls
      >>= removeIndexHtml

  create ["feed.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx <> bodyField "description"
      posts <- fmap (take 10) . recentFirst =<<
        loadAllSnapshots "articles/*" "content"
      renderAtom myFeedConfig feedCtx posts

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <>
  defaultContext

--------------------------------------------------------------------------------
myFeedConfig :: FeedConfiguration
myFeedConfig = FeedConfiguration
  { feedTitle       = "House Jeffries"
  , feedDescription = ""
  , feedAuthorName  = "Ian G. Jeffries"
  , feedAuthorEmail = "ian@housejeffries.com"
  , feedRoot        = "http://housejeffries.com"
  }

--------------------------------------------------------------------------------
-- | Create "foo/index.html" pages instead of "foo.html" pages. These show up as
-- just "foo" in the browser, which looks nicer than "foo.html".
--
-- Code from here:
-- https://raw.githubusercontent.com/Abizern/hblog/master/hblog.hs
--
-- Though he probably got the idea from here:
-- http://yannesposito.com/Scratch/en/blog/Hakyll-setup/
niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident =
      takeDirectory p </> takeBaseName p </> "index.html"
      where
        p = toFilePath ident

-- Replace an url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item

-- Removes the .html component of a URL if it is local
removeIndexStr :: String -> String
removeIndexStr url = case splitFileName url of
  (dir, "index.html") | isLocal dir -> dir
                      | otherwise   -> url
  _                                 -> url
  where
    isLocal uri = not ("://" `isInfixOf` uri)

--------------------------------------------------------------------------------
dropFirstDir :: Routes
dropFirstDir = customRoute $ dropDirectory1 . toFilePath

-- | Comments and code from here:
-- https://hackage.haskell.org/package/shake-0.3.4/docs/src/Development-Shake-FilePath.html#dropDirectory1
--
--
-- Drop the first directory from a 'FilePath'. Should only be used on
-- relative paths.
--
-- > dropDirectory1 "aaa/bbb" == "bbb"
-- > dropDirectory1 "aaa/" == ""
-- > dropDirectory1 "aaa" == ""
-- > dropDirectory1 "" == ""
dropDirectory1 :: FilePath -> FilePath
dropDirectory1 = drop 1 . dropWhile (not . Native.isPathSeparator)

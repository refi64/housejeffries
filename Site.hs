{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List             (isInfixOf)
import           Data.Monoid
import           Hakyll
import qualified System.FilePath       as Native
import           System.FilePath.Posix (replaceExtension, splitDirectories,
                                        splitFileName, takeBaseName,
                                        takeDirectory, (</>))
import           Text.Read             (readMaybe)

-- | Don't ignore any files (e.g. dotfiles like .htaccess).
myConfig :: Configuration
myConfig = defaultConfiguration { ignoreFile = const False }

myFeedConfig :: FeedConfiguration
myFeedConfig = FeedConfiguration
  { feedTitle       = "House Jeffries"
  , feedDescription = ""
  , feedAuthorName  = "Ian G. Jeffries"
  , feedAuthorEmail = "ian@housejeffries.com"
  , feedRoot        = "http://housejeffries.com"
  }

main :: IO ()
main = hakyllWith myConfig $ do

  match "resources/*" $ do
    route dropFirstDir
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "templates/*" $ compile templateCompiler

  match "_pages/1/page.md" $ do
    route $ constRoute "index.html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/base.html" (defaultContext <> pageIdContext)
      >>= relativizeUrls

  -- Output pages as HTML.
  match "_pages/*/page.md" $ do
    route $ customRoute (\x -> let pageId = unsafePageId . splitDirectories . toFilePath $ x
                               in "page" </> pageId </> "index.html")
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/page.html" pageContext
      >>= saveSnapshot "pageSnapshot"
      >>= loadAndApplyTemplate "templates/base.html" pageContext
      >>= relativizeUrls
      >>= removeIndexHtml

  match "_pages/*/*" $ do
    route $ customRoute (("page" </>) . dropDirectory1 . toFilePath)
    compile copyFileCompiler

  create ["feed.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = pageContext <> bodyField "description"
      pages <- recentFirst =<< loadAllSnapshots ("_pages/*" .&&. hasNoVersion) "pageSnapshot"
      renderAtom myFeedConfig feedCtx pages

pageContext :: Context String
pageContext = dateField "published" "%B %e, %Y" -- e.g. February 3, 2015
           <> pageIdContext
           <> defaultContext

unsafePageId :: [FilePath] -> String
unsafePageId xs =
  let pageId = xs !! 1
  in case readMaybe pageId :: Maybe Int of
    Nothing -> error $ "Page ID isn't an integer: " <> pageId
    Just _  -> pageId

pageIdContext :: Context a
pageIdContext = field "pageId" $ return . unsafePageId . splitDirectories . toFilePath . itemIdentifier

--------------------------------------------------
-- * Index trick
--------------------------------------------------

-- | Create "foo/index.html" pages instead of "foo.html" pages. These show up as
-- just "foo" in the browser, which looks nicer than "foo.html".
--
-- Code from here:
-- https://raw.githubusercontent.com/Abizern/hblog/master/hblog.hs
--
-- Though he probably got the idea from here:
-- http://yannesposito.com/Scratch/en/blog/Hakyll-setup/
indexTrick :: Routes
indexTrick = customRoute createIndexRoute
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

--------------------------------------------------
-- * General tools
--------------------------------------------------

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

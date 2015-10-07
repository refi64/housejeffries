{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- * The biggest things going down here are URL hacks.
--
-- I want HTML pages to have nice URLs without trailing slashes.
-- URLs with a slash should redirect to a URL without one:
--
--   http://housejeffries.com/page/4
--
-- I want page.md files to have relative links to resources within them:
--
--   ./some-image.jpg
--
-- I want both of these things to work at the same time.
--
-- To get this I do the following:
--
-- A) I have hakyll write HTML files to foo/index.html instead of foo.html.
--
-- B) I have rewrite rules in resources/.htaccess. These redirect
-- foo/index.html and foo/ to foo.
--
-- Note that this means there can be no document-relative links
-- in the deployed site, because the foo/ to foo change moves them
-- up a directory.
--
-- C) I rewrite all links within HTML pages to be site-root relative.
-- This way they work on both local and public servers (unlike absolute
-- links) and they survive the .htaccess redirects (unlike document-relative
-- links).

module Main where

import           Control.Monad
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
    compile $ do
      -- This strategy for embedding template language directly into the markdown
      -- file comes from here:
      --
      --     https://groups.google.com/d/msg/hakyll/ooMEwuiQZ24/dbWzHhGxuRIJ
      pages <- recentFirst =<< loadAll (complement "_pages/1/page.md" .&&.  "_pages/*/page.md")
      let ctx = pageIdContext
             <> listField "pages" pageContext (return pages)
             <> boolField "isHomepage" (const True)
             <> defaultContext
      getResourceBody
        >>= applyAsTemplate ctx
        >>= renderPandoc
        >>= loadAndApplyTemplate "templates/base.html" ctx
        >>= removeIndexFromLinks

  match "_pages/*/page.md" $ do
    route $ customRoute (\x -> let pageId = unsafePageId . splitDirectories . toFilePath $ x
                               in "page" </> pageId </> "index.html")
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/page.html" pageContext
      >>= removeIndexFromLinks
      >>= siteRelativeUrls -- Must come before saveSnapshot so it will be applied to feed content.
      >>= saveSnapshot "pageSnapshot"
      >>= loadAndApplyTemplate "templates/base.html" pageContext

  match "_pages/*/*" $ do
    route $ customRoute (("page" </>) . dropDirectory1 . toFilePath)
    compile copyFileCompiler

  create ["feed.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = pageContext <> bodyField "description"
      let match = complement "_pages/1/page.md" .&&. "_pages/*/page.md" .&&. hasNoVersion
      pages <- recentFirst =<< loadAllSnapshots match "pageSnapshot"

      -- The feed's usual failure mode is to silently not include any articles.
      when (length pages < 1) $ error "No items in Atom feed."

      renderAtom myFeedConfig feedCtx pages

--------------------------------------------------
-- * Page Ids
--------------------------------------------------

pageContext :: Context String
pageContext = dateField "published" "%B %e, %Y" -- e.g. February 3, 2015
           <> pageIdContext
           <> defaultContext -- Goes last so that it will be overwritten in case of conflict.

pageIdContext :: Context a
pageIdContext = field "pageId" $ return . unsafePageId . splitDirectories . toFilePath . itemIdentifier

unsafePageId :: [FilePath] -> String
unsafePageId xs =
  let pageId = xs !! 1
  in case readMaybe pageId :: Maybe Int of
    Nothing -> error ("Page ID isn't an integer: " <> pageId)
    Just _  -> pageId

--------------------------------------------------
-- * Site Relativization
--------------------------------------------------

-- Rewrite document-relative URLs to site-relative URLs.
--
-- NOTE: This doesn't work for document-relative URLs
-- other than ones starting with "./" (e.g. not "../").
siteRelativeUrls :: Item String -> Compiler (Item String)
siteRelativeUrls item = do
  route <- getRoute (itemIdentifier item)
  case route of
    Nothing -> error "No route for Item."
    Just a  -> return (siteRelativize a <$> item)

-- NOTE: This doesn't work for document-relative URLs
-- other than ones starting with "./" (e.g. not "../").
siteRelativize
  :: String -- | Identifier path
  -> String -- | HTML to relativize
  -> String -- | Resulting HTML
siteRelativize path = withUrls f
  where
    f ('.':'/':xs) = "/" <> removeIndexStr path <> xs
    f xs           = xs

--------------------------------------------------
-- * Index trick
--------------------------------------------------

-- | Replace local URLs of the form foo/bar/index.html with foo/bar.
removeIndexFromLinks :: Item String -> Compiler (Item String)
removeIndexFromLinks item = return $ fmap (withUrls removeIndexStr) item

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
dropFirstDir = customRoute (dropDirectory1 . toFilePath)

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

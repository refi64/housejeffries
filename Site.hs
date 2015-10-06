{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- * The biggest thing going down here is a URL hack.
--
-- I don't want index.html to show up in URLs.
--
-- I want pages to have nice URLs without trailing slashes.
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
-- In order to get the first I have rewrite rules in resources/.htaccess.
-- Note that that this takes place on the server, so just because things
-- are working fine locally doesn't mean they will be after pushing.
--
-- This however breaks relative links to other resources. A link such as
-- ./image.jpg (that resolves to /pages/2/image.jpg) becomes a link
-- that resolves to /pages/image.jpg. My first way to fix this was just
-- to write the link to ./2/image.jpg, but that was sad.
--
-- Currently I use the <base> URL element, but only in non-homepage pages.
--
-- Note that <base> affects both anchor links within documents
-- as well as links to other documents, but it seems to be working
-- at the moment. I think I'm in good shape since I'm just setting
-- the resolving path to where the resource actually lives, not using
-- <base> to lie about where it's located.
--
-- Also note that Hakyll does some rewriting too, so /css/minimal.css
-- in the template becomes ../../css/minimal.css when viewed in an
-- output file such as page/2/index.html. TODO: find out why it does
-- this.

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
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/base.html" homepageContext
      >>= relativizeUrls

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
      let match = complement "_pages/1/page.md" .&&. "_pages/*/page.md" .&&. hasNoVersion
      pages <- recentFirst =<< loadAllSnapshots match "pageSnapshot"

      -- The feed's usual failure mode is to silently not include any articles.
      when (length pages < 1) $ error "No items in Atom feed."

      renderAtom myFeedConfig feedCtx pages

homepageContext :: Context String
homepageContext = pageIdContext
               <> boolField "isHomepage" (const True)
               <> defaultContext -- Goes last so that it will be overwritten in case of conflict.

pageContext :: Context String
pageContext = dateField "published" "%B %e, %Y" -- e.g. February 3, 2015
           <> pageIdContext
           <> defaultContext

pageIdContext :: Context a
pageIdContext = field "pageId" $ return . unsafePageId . splitDirectories . toFilePath . itemIdentifier

unsafePageId :: [FilePath] -> String
unsafePageId xs =
  let pageId = xs !! 1
  in case readMaybe pageId :: Maybe Int of
    Nothing -> error $ "Page ID isn't an integer: " <> pageId
    Just _  -> pageId

--------------------------------------------------
-- * Index trick
--------------------------------------------------

-- | Create "foo/index.html" pages instead of "foo.html" pages.
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

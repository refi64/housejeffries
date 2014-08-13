--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import Data.List (isInfixOf)
import System.FilePath.Posix  (takeBaseName,takeDirectory,(</>),splitFileName)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateCompiler

    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

    match "index.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/base.html" defaultContext
            >>= relativizeUrls

    match "articles/*" $ do
        route $ niceRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/base.html" postCtx
            >>= relativizeUrls
            >>= removeIndexHtml

    create ["feed.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "articles/*" "content"
            renderAtom myFeedConfiguration feedCtx posts

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

--------------------------------------------------------------------------------
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "House Jeffries"
    , feedDescription = ""
    , feedAuthorName  = "Ian G. Jeffries"
    , feedAuthorEmail = "ian@housejeffries.com"
    , feedRoot        = "http://housejeffries.com"
    }

--------------------------------------------------------------------------------
-- Create "foo/index.html" pages instead of "foo.html" pages. These show up as
-- just "foo" in the browser, which looks nicer than "foo.html".

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

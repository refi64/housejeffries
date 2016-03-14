
module Main where

import           Data.List                  (isInfixOf)
import           Data.Maybe                 (mapMaybe)
import           Data.Monoid
import           Data.Traversable           (for)
import           Development.Shake
import           Development.Shake.FilePath (dropDirectory1, takeDirectory1,
                                             (</>))
import           Text.Read                  (readMaybe)

import           Feed
import           Lib
import           Types

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = "_build" } rules

rules :: Rules ()
rules = do

    phony "clean" $ do
        alwaysRerun
        return ()
        removeFilesAfter "_build" ["//*"]

    getTemplates <- newCache $ \() -> do
        let f = liftIO . getTemplate . ("src/templates" </>)
        SiteTemplates
            <$> f "base.html"
            <*> f "page-core.html"
            <*> f "page-after-rss-serialization.html"

    getPage <- newCache $ \pageId -> do
        templates <- getTemplates ()
        liftIO (getPageUncached templates pageId)

    let getPages = fmap reverse . traverse getPage =<< getPids

    -- 'want', but determined dynamically.
    action $ do
        resources <- getDirectoryFiles "src/resources" ["//*"]
        css <- fmap ("css" </>) <$> getDirectoryFiles "src/css" ["//*"]
        pids <- getPids
        pageResources <- for pids $ \pid ->
              fmap (show pid </>)
            . filter (/= "page.md")
            . filter (not . isInfixOf ".git")
            . filter (not . isInfixOf ".stack-work")
            <$> getDirectoryFiles (pageDir </> show pid) ["//*"]
        need $ "clean" : fmap (destDir </>)
            ( "index.html"
            : "feed.xml"
            : resources
            <> css
            <> [ "page" </> show pid </> "index.html" | pid <- pids]
            <> fmap ("page" </>) (concat pageResources)
            )

    -- write the homepage
    destDir </> "index.html" %> \out -> do
        templates <- getTemplates ()
        pages     <- getPages
        liftIO (writeHomepage templates pages out)

    -- write each of the wiki pages
    priority 3 $ destDir </> "page/*/index.html" %> \out -> do
        let pageId = read . takeDirectory1 . dropDirectory1
                   . dropDirectory1 $ out
        page      <- getPage pageId
        templates <- getTemplates ()
        liftIO (writePage templates page out)

    -- copy over images etc. for each wiki page
    priority 2 $ destDir </> "page//*" %> \out ->
        copyFile' (pageDir </> dropDirectory1 (dropDirectory1 out)) out

    destDir </> "feed.xml" %> \out -> writeFile' out . feed =<< getPages

    priority 2 $ "_site/css/*" %> \out ->
        copyFile' ("src" </> dropDirectory1 out) out

    -- copy over site resources like favicon.ico
    "_site//*" %> \out ->
        copyFile' ("src/resources" </> dropDirectory1 out) out

getPids :: Action [Int]
getPids = filter (/= 1) . mapMaybe readMaybe <$> getDirectoryDirs pageDir

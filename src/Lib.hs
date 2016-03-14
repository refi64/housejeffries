
module Lib where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Default         (def)
import qualified Data.HashMap.Strict  as H
import           Data.Maybe           (fromJust)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Hakyll               (withUrls)
import           System.FilePath
import           Text.Pandoc          (Pandoc, Template)
import qualified Text.Pandoc          as P

import           Types

writeHomepage :: SiteTemplates -> [Page] -> String -> IO ()
writeHomepage templates pages out = do
    index <- getTemplate (pageDir </> "1/page.md")
    let pageArray = (\page -> object
                        [ "url"   .= ("/page" </> show (_pageId page))
                        , "title" .= _metaTitle (_pageMeta page)
                        ]
                    ) <$> pages
        pandoc = readMarkdown
               . P.renderTemplate index
               $ object ["page" .= pageArray]
    T.writeFile out $ P.renderTemplate (_templateBase templates)
        (object [ "body"   .= P.writeHtmlString def pandoc
                , "pageId" .= String "1"
                ])

writePage :: SiteTemplates -> Page -> String -> IO ()
writePage templates page out =
    T.writeFile out $ P.renderTemplate
                          (_templateBase templates)
                          (object [ "body"   .= String body
                                  , "pageId" .= show (_pageId page)
                                  ])
  where
    body :: Text
    body = P.renderTemplate (_templatePageAfterRSS templates)
         $ object ["body" .= P.writeHtmlString def (_pageContent page)]

getPageUncached :: SiteTemplates -> Int -> IO Page
getPageUncached templates pageId = do
    t <- T.pack
       . siteRelativize ("/page" </> show pageId)
       . P.writeHtmlString def
       . readMarkdown
       <$> readFile ((pageDir </> show pageId) </> "page.md")
    meta <- fromJust . decode <$> B.readFile ((pageDir </> show pageId)
                                                       </> "meta.json")
    let Object metaHm = toJSON meta
        pandoc = readHtml
               . P.renderTemplate (_templatePageCore templates)
               $ H.insert "body" (String t) metaHm
    pure (Page pageId pandoc meta)

readMarkdown :: String -> Pandoc
readMarkdown = either (error . show) id
             . P.readMarkdown def { P.readerSmart = True }

readHtml :: String -> Pandoc
readHtml = either (error . show) id . P.readHtml def

getTemplate :: String -> IO Template
getTemplate = fmap (either (error . show) id . P.compileTemplate)
            . T.readFile

-- NOTE: This doesn't work for document-relative URLs
-- other than ones starting with "./" (e.g. not "../").
siteRelativize
  :: String
     -- ^ Identifier path
  -> String
     -- ^ HTML to relativize
  -> String
     -- ^ Resulting HTML
siteRelativize path = withUrls f
  where
    f ('.':'/':xs) = path </> xs
    f xs           = xs

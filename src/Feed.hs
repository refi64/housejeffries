
module Feed (feed) where

import           Data.Default          (def)
import           Data.Time
import           Data.Time.RFC3339     (formatTimeRFC3339)
import           System.FilePath
import qualified Text.Atom.Feed        as Atom
import qualified Text.Atom.Feed.Export as Atom
import qualified Text.Pandoc           as P
import qualified Text.XML.Light.Output as XML

import           Types

feed :: [Page] -> String
feed pages =
    XML.ppElement . Atom.xmlFeed $ fd
        { Atom.feedEntries = toEntry <$> pages
        , Atom.feedLinks   = [Atom.nullLink "http://housejeffries.com"]
        }
  where
    fd :: Atom.Feed
    fd = Atom.nullFeed
             "http://housejeffries.com/feed.xml" -- ID
             (Atom.TextString "House Jeffries")  -- Title
             (case pages of                      -- Updated
                 page:_ -> rfc3339 . _metaDate . _pageMeta $ page
                 _      -> mempty)

    toEntry :: Page -> Atom.Entry
    toEntry (Page pid content meta) =
        (Atom.nullEntry
            link -- The ID field, but must be a link to validate.
            (Atom.TextString . show . _metaTitle $ meta)
            (rfc3339 (_metaDate meta)))
        { Atom.entryAuthors = [Atom.nullPerson { Atom.personName = myName }]
        , Atom.entryLinks   = [Atom.nullLink link]
        , Atom.entryContent = Just . Atom.HTMLContent
                            . P.writeHtmlString def $ content
        }
      where
        link :: String
        link = "http://housejeffries.com/page" </> show pid

-- | Required to pass <https://www.w3.org/> validation.
rfc3339 :: Day -> String
rfc3339 d = formatTimeRFC3339 $ utcToZonedTime utc (UTCTime d noon)
  where
    noon = 86400 / 2

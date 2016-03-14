
module Types where

import           Data.Aeson
import           Data.Text   (Text)
import           Data.Time
import           Text.Pandoc (Pandoc, Template)

myName :: String
myName = "Ian Grant Jeffries"

pageDir :: FilePath
pageDir = "/home/traveller/code/mine/hjpages"

destDir :: FilePath
destDir = "_site"

data Page = Page
    { _pageId      :: Int
    , _pageContent :: Pandoc
    , _pageMeta    :: Meta
    }

data Meta = Meta
    { _metaTitle :: Text
    , _metaDate  :: Day
    }

instance FromJSON Meta where
    parseJSON = withObject "Meta" $ \o -> Meta
        <$> o .: "title"
        <*> fmap read (o .: "published")

instance ToJSON Meta where
    toJSON meta = object [ "title"     .= _metaTitle meta
                         , "published" .= show (_metaDate meta)
                         ]

data SiteTemplates = SiteTemplates
    { _templateBase         :: Template
    , _templatePageCore     :: Template
    , _templatePageAfterRSS :: Template
    }

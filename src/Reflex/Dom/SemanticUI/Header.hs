{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Reflex.Dom.SemanticUI.Header where

import           Control.Monad (void)
import           Control.Monad.Trans (liftIO)
import           Control.Lens ((^.))
import           Data.Default (Default (def))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified GHCJS.DOM.Element as DOM
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core hiding
  ( checkbox, Checkbox (..), checkbox_value, checkbox_change
  , CheckboxConfig (..), checkboxConfig_attributes, checkboxConfig_setValue
  )

import Debug.Trace

import Reflex.Dom.SemanticUI.Icon
import Reflex.Dom.SemanticUI.Common (jQuery, UiClassText(..), consoleLog, Size(..), UI (..), Floated(..)
  )

data ImageConfig = ImageConfig
  { _size :: Maybe Size
  , _rounded :: Maybe ImageRounded
  }

instance Default ImageConfig where
  def = ImageConfig
    { _size = Nothing
    , _rounded = Nothing
    }

data Image = Image
  { _src :: Text
  , _config :: ImageConfig
  }

data ImageRounded = Rounded | Circular deriving (Eq, Show)

imageRounded :: ImageRounded -> Text
imageRounded = T.toLower . T.pack . show

instance UI t m Image where
  type Return t m Image = ()
  ui (Image src ImageConfig {..}) = elAttr "img" attrs blank
    where
      attrs = "src" =: src <> "class" =: T.unwords classes
      classes = imgRoundedClass $ imgSizeClass [ "ui", "image" ]
      imgSizeClass = maybe id ((:) . uiText) _size
      imgRoundedClass = maybe id ((:) . imageRounded) _rounded

data HeaderConfig m = HeaderConfig
  { _image :: Maybe Image
  , _icon :: Maybe Icon
  , _subHeader :: Maybe (m ())
  , _header :: HeaderType
  , _dividing :: Bool
  , _floated :: Maybe Floated
  }

instance Default (HeaderConfig m) where
  def = HeaderConfig
    { _image = Nothing
    , _icon = Nothing
    , _subHeader = Nothing
    , _header = PageHeader
    , _dividing = False
    , _floated = Nothing
    }

headerConfigClasses :: HeaderConfig m -> [Text]
headerConfigClasses HeaderConfig {..} = catMaybes
  [ justWhen _dividing "dividing"
  , uiText <$> _floated
  ]

data HeaderType = PageHeader | ContentHeader

data HeaderSize = H1 | H2 | H3 | H4 | H5 deriving (Eq, Show)

headerSizeEl :: HeaderSize -> Text
headerSizeEl H1 = "h1"
headerSizeEl H2 = "h2"
headerSizeEl H3 = "h3"
headerSizeEl H4 = "h4"
headerSizeEl H5 = "h5"

headerSize :: HeaderSize -> Text
headerSize H1 = "huge"
headerSize H2 = "large"
headerSize H3 = "medium"
headerSize H4 = "small"
headerSize H5 = "tiny"

-- | Create a header.
--
-- https://semantic-ui.com/elements/header.html
data Header m a = Header
  { _size :: HeaderSize
  , _content :: m a
  , _config :: HeaderConfig m
  }

type Href = Text
data Anchor m a = Anchor Href (m a)
instance m ~ m' => UI t m' (Anchor m a) where
  type Return t m' (Anchor m a) = (Event t (), a)
  ui (Anchor href inner) = do
    (a, b) <- elAttr' "a" ("href" =: href <> "class" =: "ui anchor") inner
    return (domEvent Click a, b)

instance m ~ m' => UI t m' (Header m a) where
  type Return t m' (Header m a) = a
  ui (Header size widget config@HeaderConfig {..}) = case _header of
    PageHeader -> elClass (headerSizeEl size) (T.unwords classes) iContent
    ContentHeader -> divClass (T.unwords $ headerSize size : classes) iContent
    where
      -- TODO: make this like the icons
      classes = "ui" : "header" : headerConfigClasses config
      iContent
        | Just img <- _image = ui img >> content
        | Just icon <- _icon = ui icon >> content
        | otherwise = content
      content
        | Just sub <- _subHeader = divClass "content" $ do
            a <- widget
            divClass "sub header" sub
            return a
        | otherwise = widget

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
import Reflex.Dom.SemanticUI.Common

data ImageConfig = ImageConfig
  { _size :: Maybe Size
  , _rounded :: Maybe ImageRounded
  , _avatar :: Bool
  }

instance Default ImageConfig where
  def = ImageConfig
    { _size = Nothing
    , _rounded = Nothing
    , _avatar = False
    }

imageConfigClasses :: ImageConfig -> [Text]
imageConfigClasses ImageConfig {..} = catMaybes
  [ uiText <$> _size
  , uiText <$> _rounded
  , justWhen _avatar "avatar"
  ]

data Image = Image
  { _src :: Text
  , _config :: ImageConfig
  }

data ImageRounded = Rounded | Circular deriving (Eq, Show)

instance UiClassText ImageRounded where
  uiText Rounded = "rounded"
  uiText Circular = "circular"

instance UI t m Image where
  type Return t m Image = ()
  ui (Image src config@ImageConfig {..}) = elAttr "img" attrs blank
    where
      attrs = "src" =: src <> "class" =: T.unwords classes
      classes = "ui" : "image" : imageConfigClasses config

data HeaderConfig m = HeaderConfig
  { _image :: Maybe Image
  , _icon :: Maybe Icon
  , _subHeader :: Maybe (m ())
  , _header :: HeaderType
  , _dividing :: Bool
  , _floated :: Maybe Floated
  , _item :: Bool
  , _component :: Bool -- This is the "ui" (component) class
  }

instance Default (HeaderConfig m) where
  def = HeaderConfig
    { _image = Nothing
    , _icon = Nothing
    , _subHeader = Nothing
    , _header = PageHeader
    , _dividing = False
    , _floated = Nothing
    , _item = False
    , _component = True
    }

headerConfigClasses :: HeaderConfig m -> [Text]
headerConfigClasses HeaderConfig {..} = catMaybes
  [ justWhen _dividing "dividing"
  , justWhen _item "item"
  , justWhen _component "ui"
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

data Paragraph = Paragraph
  { _text :: Text
  }

instance ToPart Paragraph where
  toPart = id

instance UI t m Paragraph where
  type Return t m Paragraph = ()
  ui (Paragraph txt) = el "p" $ text txt

-- | Create a header.
--
-- https://semantic-ui.com/elements/header.html
data Header m a = Header
  { _size :: HeaderSize
  , _content :: m a
  , _config :: HeaderConfig m
  }

instance Item (Header m a) where
  toItem (Header size content config) = Header size content $
    config { _item = True, _component = False }

instance ToPart (Header m a) where
  toPart (Header size content config) = Header size content $
    config { _component = False }

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
      classes = "header" : headerConfigClasses config
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

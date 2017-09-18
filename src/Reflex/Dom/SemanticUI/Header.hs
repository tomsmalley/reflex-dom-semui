{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Reflex.Dom.SemanticUI.Header where

import           Control.Monad (void)
import           Control.Monad.Trans (liftIO)
import           Control.Lens ((^.))
import           Data.Default (Default (def))
import           Data.Map (Map)
import qualified Data.Map as M
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

import Reflex.Dom.SemanticUI.Common (jQuery, UiClassText(..), consoleLog)

data ImageConfig = ImageConfig
  { _size :: Maybe ImageSize
  , _rounded :: Maybe ImageRounded
  }

instance Default ImageConfig where
  def = ImageConfig
    { _size = Nothing
    }

data Image = Image
  { _src :: Text
  , _config :: ImageConfig
  }

data ImageRounded = Rounded | Circular deriving (Eq, Show)

imageRounded :: ImageRounded -> Text
imageRounded = T.toLower . T.pack . show

class UI a where
  ui :: MonadWidget t m => a -> m ()

instance UI Image where
  ui (Image src ImageConfig {..}) = elAttr "img" attrs blank
    where
      attrs = "src" =: src <> "class" =: T.unwords classes
      classes = imgRoundedClass $ imgSizeClass [ "ui", "image" ]
      imgSizeClass = maybe id ((:) . imageSize) _size
      imgRoundedClass = maybe id ((:) . imageRounded) _rounded

data ImageSize = Mini | Tiny | Small | Medium | Large | Big | Huge | Massive deriving (Eq, Show)

imageSize :: ImageSize -> Text
imageSize = T.toLower . T.pack . show

data HeaderConfig = HeaderConfig
  { _image :: Maybe Image
  , _subHeader :: Maybe Text
  , _header :: HeaderType
  }

instance Default HeaderConfig where
  def = HeaderConfig
    { _image = Nothing
    , _subHeader = Nothing
    , _header = PageHeader
    }

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
data Header = Header
  { _size :: HeaderSize
  , _text :: Text
  , _config :: HeaderConfig
  }

instance UI Header where
  ui (Header size txt HeaderConfig {..}) = case _header of
    PageHeader -> elClass (headerSizeEl size) (T.unwords classes) $ case _image of
      Nothing -> text txt
      Just img -> do
        ui img
        divClass "content" $ do
          text txt
          maybe blank (divClass "sub header" . text) _subHeader
    ContentHeader -> divClass (T.unwords $ headerSize size : classes) $ text txt
    where
      classes = [ "ui", "header" ]


{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DuplicateRecordFields                      #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE RecursiveDo              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

module Reflex.Dom.SemanticUI.Icon where

------------------------------------------------------------------------------
import           Data.Default
import           Data.Maybe
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex.Dom.Core hiding (fromJSString)
import Data.Maybe (catMaybes)
------------------------------------------------------------------------------
import           Reflex.Dom.SemanticUI.Common
------------------------------------------------------------------------------

data HorizontalAttached = LeftAttached | RightAttached deriving (Eq, Show)
data VerticalAttached = TopAttached | BottomAttached deriving (Eq, Show)

instance UiClassText VerticalAttached where
  uiText TopAttached = "top"
  uiText BottomAttached = "bottom"

instance UiClassText HorizontalAttached where
  uiText LeftAttached = "left"
  uiText RightAttached = "right"

combineAttached :: Maybe VerticalAttached -> Maybe HorizontalAttached -> Maybe Text
combineAttached Nothing Nothing = Nothing
combineAttached mv mh = Just $ T.unwords $ catMaybes [ uiText <$> mv, uiText <$> mh, Just "attached" ]

data Label = Label Text LabelConfig
data LabelConfig = LabelConfig
  { _vAttached :: Maybe VerticalAttached
  , _hAttached :: Maybe HorizontalAttached
  , _link :: Bool
  }

instance Default LabelConfig where
  def = LabelConfig
    { _vAttached = Nothing
    , _hAttached = Nothing
    , _link = False
    }

labelConfigClasses :: LabelConfig -> [Text]
labelConfigClasses LabelConfig {..} = catMaybes
  [ combineAttached _vAttached _hAttached
  ]

instance UI t m Label where
  type Return t m Label = Event t ()

  ui (Label txt config@LabelConfig {..}) = do
    (e, _) <- elAttr' elType ("class" =: T.unwords classes) $ text txt
    return $ domEvent Click e
      where
        classes = "ui" : "label" : labelConfigClasses config
        elType = if _link then "a" else "div"

data Icon
  = Icon Text IconConfig
  | Icons [Icon] IconsConfig

data IconConfig = IconConfig
  { _disabled :: Bool
  , _loading :: Bool
  , _fitted :: Bool
  , _size :: Maybe Size
  , _link :: Bool
  , _floated :: Maybe Floated
  , _title :: Maybe Text
--  , _flipped :: Bool
--  , _rotated :: Bool
--  , _circular :: Bool
--  , _bordered :: Bool
  , _inverted :: Bool
  , _color :: Maybe Color
  }

instance Default IconConfig where
  def = IconConfig
    { _disabled = False
    , _loading = False
    , _fitted = False
    , _size = Nothing
    , _link = False
    , _floated = Nothing
    , _title = Nothing
    , _inverted = False
    , _color = Nothing
    }

iconConfigClasses :: IconConfig -> [Text]
iconConfigClasses IconConfig {..} = catMaybes
  [ justWhen _disabled "disabled"
  , justWhen _loading "loading"
  , justWhen _fitted "fitted"
  , justWhen _link "link"
  , justWhen _inverted "inverted"
  , uiText <$> nothingIf Medium _size
  , uiText <$> _floated
  , uiText <$> _color
  ]

nothingIf :: Eq a => a -> Maybe a -> Maybe a
nothingIf x (Just y) | x == y = Nothing
nothingIf _ m = m

justWhen :: Bool -> a -> Maybe a
justWhen True = Just
justWhen False = const Nothing

data IconsConfig = IconsConfig
  { _size :: Maybe Size
  }

instance Default IconsConfig where
  def = IconsConfig
    { _size = Nothing
    }

--instance UI t m Icons where
--  type Return t m Icons = ()
--  ui (Icons IconsConfig {..} icons)
--    = elAttr "i" ("class" =: T.unwords classes) $ mapM_ ui icons
--      where classes = "icons" : maybe [] (pure . uiTextSize) _size

instance UI t m Icon where
  type Return t m Icon = Event t ()

  ui (Icon icon config@IconConfig {..}) = do
    (e, _) <- elAttr' "i" ("class" =: T.unwords classes <> mtitle) blank
    return $ domEvent Click e
      where classes = icon : "icon" : iconConfigClasses config
            mtitle
              | Just title <- _title = "title" =: title
              | otherwise = mempty

  ui (Icons icons IconsConfig {..})
    = elAttr "i" ("class" =: T.unwords classes) $ leftmost <$> traverse ui icons
      where classes = "icons" : maybe [] (pure . uiText) _size

data UiIcon = UiIcon
    { _uiIcon_size       :: Maybe UiSize
    , _uiIcon_color      :: Maybe UiColor
    , _uiIcon_disabled   :: Maybe UiDisabled
    , _uiIcon_left       :: Maybe UiLeft
    , _uiIcon_loading    :: Maybe UiLoading
    , _uiIcon_fitted     :: Maybe UiFitted
    , _uiIcon_flipped    :: Maybe UiFlipped
    , _uiIcon_rotated    :: Maybe UiRotated
    , _uiIcon_circular   :: Maybe UiCircular
    , _uiIcon_bordered   :: Maybe UiBordered
    , _uiIcon_inverted   :: Maybe UiInverted
    } deriving (Eq,Show)

instance Default UiIcon where
  def = UiIcon def def def def def def def def def def def

instance UiHasSize UiIcon where
  uiSetSize s b = b { _uiIcon_size = Just s }

instance UiHasColor UiIcon where
  uiSetColor c b = b { _uiIcon_color = Just c }

instance UiHasDisabled UiIcon where
  disabled b = b { _uiIcon_disabled = Just UiDisabled }

instance UiHasLeft UiIcon where
  uiLeft b = b { _uiIcon_left = Just UiLeft }

instance UiHasLoading UiIcon where
  loading b = b { _uiIcon_loading = Just UiLoading }

instance UiHasFitted UiIcon where
  fitted b = b { _uiIcon_fitted = Just UiFitted }

instance UiHasFlipped UiIcon where
  uiSetFlipped s b = b { _uiIcon_flipped = Just s }

instance UiHasRotated UiIcon where
  uiSetRotated s b = b { _uiIcon_rotated = Just s }

instance UiHasCircular UiIcon where
  circular b = b { _uiIcon_circular = Just UiCircular }

instance UiHasBordered UiIcon where
  bordered b = b { _uiIcon_bordered = Just UiBordered }

instance UiHasInverted UiIcon where
  inverted b = b { _uiIcon_inverted = Just UiInverted }

uiIconAttrs :: UiIcon -> Text
uiIconAttrs UiIcon{..} = T.unwords $ catMaybes
    [ uiText <$> _uiIcon_size
    , uiText <$> _uiIcon_color
    , uiText <$> _uiIcon_disabled
    , uiText <$> _uiIcon_left
    , uiText <$> _uiIcon_loading
    , uiText <$> _uiIcon_fitted
    , uiText <$> _uiIcon_flipped
    , uiText <$> _uiIcon_rotated
    , uiText <$> _uiIcon_circular
    , uiText <$> _uiIcon_bordered
    , uiText <$> _uiIcon_inverted
    ]

uiIcon
    :: MonadWidget t m
    => Text
    -> Dynamic t UiIcon
    -> m (Event t ())
uiIcon ty iDyn = do
    (e,_) <- elDynAttr' "i" (mkAttrs <$> iDyn) blank
    return $ domEvent Click e
  where
    mkAttrs b = "class" =: T.unwords [ty, uiIconAttrs b, "icon"]


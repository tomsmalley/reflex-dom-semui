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

import Data.Foldable (traverse_)
import           Data.Default
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex.Dom.Core hiding (fromJSString)
import Data.Maybe (catMaybes)

import           Reflex.Dom.SemanticUI.Common

--data Flag t = Flag (Dynamic t Text)
data Flag t = Flag (Active t Text)

instance t ~ t' => UI t' m (Flag t) where
  type Return t' m (Flag t) = ()
  --ui' (Flag flagDyn) = elDynAttr' "i" (attr <$> flagDyn) blank
  ui' (Flag flagActive) = case flagActive of
    Static flag -> elAttr' "i" (attr flag) blank
    Dynamic flagDyn -> elDynAttr' "i" (attr <$> flagDyn) blank
    where
      attr flag = "class" =: (flag <> " flag")

data Icon t
  = Icon (Dynamic t Text) (IconConfig t)
  | Icons [Icon t] IconsConfig

data IconConfig t = IconConfig
  { _disabled :: Dynamic t Bool
  , _loading :: Dynamic t Bool
  , _fitted :: Dynamic t Bool
  , _size :: Dynamic t (Maybe Size)
  , _link :: Dynamic t Bool
  , _floated :: Dynamic t (Maybe Floated)
  , _title :: Dynamic t (Maybe Text)
--  , _flipped :: Bool
--  , _rotated :: Bool
--  , _circular :: Bool
--  , _bordered :: Bool
  , _inverted :: Dynamic t Bool
  , _color :: Dynamic t (Maybe Color)
  }

instance Reflex t => Default (IconConfig t) where
  def = IconConfig
    { _disabled = pure False
    , _loading = pure False
    , _fitted = pure False
    , _size = pure Nothing
    , _link = pure False
    , _floated = pure Nothing
    , _title = pure Nothing
    , _inverted = pure False
    , _color = pure Nothing
    }

iconConfigClasses :: Reflex t => IconConfig t -> Dynamic t ClassText
iconConfigClasses IconConfig {..} = mconcat
  [ memptyUnless "disabled" <$> _disabled
  , memptyUnless "loading" <$> _loading
  , memptyUnless "fitted" <$> _fitted
  , memptyUnless "link" <$> _link
  , memptyUnless "inverted" <$> _inverted
  , toClassText . nothingIf Medium <$> _size
  , toClassText <$> _floated
  , toClassText <$> _color
  ]

data IconsConfig = IconsConfig
  { _size :: Maybe Size
  }
  deriving (Eq, Show)

instance Default IconsConfig where
  def = IconsConfig
    { _size = Nothing
    }

--instance UI t m Icons where
--  type Return t m Icons = ()
--  ui (Icons IconsConfig {..} icons)
--    = elAttr "i" ("class" =: T.unwords classes) $ mapM_ ui icons
--      where classes = "icons" : maybe [] (pure . uiTextSize) _size

instance t' ~ t => UI t' m (Icon t) where
  type Return t' m (Icon t) = ()

  ui' (Icon dynIcon config@IconConfig {..}) = elDynAttr' "i" attrs blank
    where
      attrs = mkAttrs <$> dynIcon <*> iconConfigClasses config <*> _title
      mkAttrs i c t = maybe mempty ("title" =:) t
                   <> "class" =: getClass (mconcat [ClassText (Just i), "icon", c])

  ui' (Icons icons IconsConfig {..})
    = elAttr' "i" ("class" =: T.unwords classes) $ traverse_ ui_ icons
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


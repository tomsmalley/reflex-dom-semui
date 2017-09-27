{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE UndecidableInstances     #-}

module Reflex.Dom.SemanticUI.Dropdown
  (
    Dropdown (..)

  , DropdownConfig (..)
  , DropdownAction (..)

  , DropdownItem (..)
  , DropdownItemConfig (..)
  ) where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Trans
import           Control.Lens ((^.), (%~))
import           Data.Default
import           Data.Functor.Identity
import qualified Data.List as L
import           Data.Map (Map)
import           Data.Maybe (catMaybes, maybeToList, listToMaybe, mapMaybe)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import Data.These
import Data.Align
import qualified GHCJS.DOM.Element as DOM
import           Language.Javascript.JSaddle
import           Text.Read (readMaybe)
import           Reflex
--import           Reflex.Host.Class
import           Reflex.Dom.Core hiding
  ( Dropdown (..), DropdownConfig (..), Select )
------------------------------------------------------------------------------
import           Reflex.Dom.SemanticUI.Common
import           Reflex.Dom.SemanticUI.Icon
import           Reflex.Dom.SemanticUI.Header
------------------------------------------------------------------------------

data DropdownAction
  = Activate
  | Combo
  | Select
  | Hide
  deriving (Eq, Show)

instance ToJSVal DropdownAction where
  toJSVal action = valMakeString $ case action of
    Activate -> "activate"
    Combo -> "combo"
    Select -> "select"
    Hide -> "hide"

------------------------------------------------------------------------------

-- | Given a div element, tell semantic-ui to convert it to a dropdown with the
-- given options. The callback function is called on change with the currently
-- selected value.
activateDropdown :: DOM.Element -> Maybe Int -> Bool -> Bool -> DropdownAction
                 -> (Text -> JSM ()) -> JSM ()
activateDropdown e maxSel useLabels fullText dropdownAction onChange = do
  o <- obj
  o <# ("forceSelection" :: Text) $ False
  o <# ("maxSelections" :: Text) $ maxSel
  o <# ("useLabels" :: Text) $ useLabels
  o <# ("fullTextSearch" :: Text) $ fullText
  o <# ("action" :: Text) $ dropdownAction
  o <# ("onChange" :: Text) $ fun $ \_ _ [t, _, _] ->
    onChange =<< fromJSValUnchecked t
  void $ jQuery e ^. js1 ("dropdown" :: Text) o

-- | Given a dropdown element, set the value to the given list. For single
-- dropdowns just provide a singleton list.
dropdownSetExactly :: DOM.Element -> [Int] -> JSM ()
dropdownSetExactly e is
  = void $ jQuery e ^. js2 ("dropdown" :: Text) ("set exactly" :: Text) (map show is)

------------------------------------------------------------------------------

-- | Config for new semantic dropdowns
data DropdownConfig t a = DropdownConfig
  { _initialValue :: a
  , _setValue :: Event t a
  , _attributes :: Map Text Text
  , _placeholder :: Text
  , _maxSelections :: Maybe Int
  , _useLabels :: Bool
  , _fullTextSearch :: Bool
  , _search :: Bool
  , _selection :: Bool
  , _fluid :: Bool
  , _action :: DropdownAction
  , _item :: Bool
  , _textOnly :: Bool
  , _inline :: Bool
  } deriving Functor

-- TODO check that this is lawful
instance (Reflex t) => Applicative (DropdownConfig t) where
  pure a = DropdownConfig
    { _initialValue = a
    , _setValue = never
    , _attributes = mempty
    , _placeholder = mempty
    , _maxSelections = Nothing
    , _useLabels = True
    , _fullTextSearch = False
    , _search = False
    , _selection = False
    , _fluid = False
    , _action = Activate
    , _item = False
    , _textOnly = False
    , _inline = False
    }
  f <*> a = a
    { _initialValue = (_initialValue f) (_initialValue a)
    , _setValue = fmapMaybe id
        $ fmap (these (const Nothing) (const Nothing) (\f a -> Just $ f a))
        $ align (_setValue f) (_setValue a)
    }

instance (Reflex t) => Default (DropdownConfig t (Maybe a)) where
  def = pure Nothing

instance (Reflex t) => Default (DropdownConfig t [a]) where
  def = pure []

dropdownConfigClasses :: DropdownConfig t a -> [Text]
dropdownConfigClasses DropdownConfig {..} = catMaybes
  [ justWhen _search "search"
  , justWhen _fluid "fluid"
  , justWhen _selection "selection"
  , justWhen _item "item"
  , justWhen _inline "inline"
  ]

-- | Helper function
indexToItem :: [DropdownItem a] -> Text -> Maybe a
indexToItem items i' = do
  i <- readMaybe $ T.unpack i'
  _value <$> items !? i

-- | Custom Dropdown item configuration
data DropdownItemConfig = DropdownItemConfig
  { _icon :: Maybe Icon
  , _image :: Maybe Image
  , _dataText :: Maybe Text
  , _flag :: Maybe Flag
  }
--  { dataText :: T.Text
--    -- ^ dataText (shown for the selected item)
--  , _
--  }
instance Default DropdownItemConfig where
  def = DropdownItemConfig
    { _icon = Nothing
    , _image = Nothing
    , _dataText = Nothing
    , _flag = Nothing
    }

data DropdownItem a = DropdownItem
  { _value :: a
  , _label :: Text
  , _config :: DropdownItemConfig
  }

data Dropdown f t a = Dropdown
  { _items :: [DropdownItem a]
  , _config :: DropdownConfig t (f a)
  }

instance (t ~ t', Eq a) => UI t' m (Dropdown Maybe t a) where
  type Return t' m (Dropdown Maybe t a) = Dynamic t (Maybe a)
  ui (Dropdown items config@DropdownConfig{..}) = do
    evt <- dropdownInternal items False config
    holdDyn def $ listToMaybe <$> evt

instance (t ~ t', Eq a) => UI t' m (Dropdown [] t a) where
  type Return t' m (Dropdown [] t a) = Dynamic t [a]
  ui (Dropdown items config@DropdownConfig{..}) = do
    holdDyn def =<< dropdownInternal items True config

instance (t ~ t', Eq a) => UI t' m (Dropdown Identity t a) where
  type Return t' m (Dropdown Identity t a) = Dynamic t a
  ui (Dropdown items config@DropdownConfig{..}) = do
    holdDyn (runIdentity _initialValue) . fmap f =<< dropdownInternal items False config'
      where f (a : _) = a
            f _ = runIdentity _initialValue
            -- Ignore attempts to set the value to an item not in the list
            config' = config { _setValue = ffilter (itemExists items . runIdentity) _setValue }

itemExists :: Eq a => [DropdownItem a] -> a -> Bool
itemExists [] _ = False
itemExists (DropdownItem {..}:as) a = if _value == a then True else itemExists as a

-- | Internal function with shared behaviour
dropdownInternal :: forall t m a f. (Foldable f, MonadWidget t m, Eq a)
  => [DropdownItem a]             -- ^ Items
  -> Bool                         -- ^ Is multiple dropdown
  -> DropdownConfig t (f a)       -- ^ Dropdown config
  -> m (Event t [a])
dropdownInternal items isMulti conf@DropdownConfig {..} = do

  (divEl, _) <- elAttr' "div" ("class" =: T.unwords classes <> _attributes) $ do

    -- This holds the placeholder. Initial value must be set by js function in
    -- wrapper.
    if _action == Activate
    then divClass "default text" $ text _placeholder
    else text _placeholder -- No wrapper if the text doesn't get replaced by the action
    elAttr "i" ("class" =: "dropdown icon") blank

    -- Dropdown menu
    divClass "menu" $ sequence_ $ imap putItem items

  -- Setup the event and callback function for when the value is changed
  (onChangeEvent, onChangeCallback) <- newTriggerEvent

  pb <- getPostBuild
  -- Activate the dropdown after element is built
  let activate = activateDropdown (_element_raw divEl) maxSel _useLabels _fullTextSearch _action
               $ liftIO . onChangeCallback
  performEvent_ $ liftJSM activate <$ pb
  -- Set initial value
  let setDropdown = liftJSM . dropdownSetExactly (_element_raw divEl)
                  . getIndices
  performEvent_ $ setDropdown _initialValue <$ pb
  -- setValue events
  performEvent_ $ setDropdown <$> _setValue

  let indices = mapMaybe (indexToItem items) . T.splitOn "," <$> onChangeEvent

  return indices

  where
    maxSel = if isMulti then _maxSelections else Nothing
    classes = "ui" : "dropdown" : (if isMulti then "multiple" else "") : dropdownConfigClasses conf
    itemDiv i a = elAttr "div"
      ("class" =: "item" <> "data-value" =: tshow i <> a)
    putItem i (DropdownItem _ t DropdownItemConfig {..}) =
      let attrs = "class" =: "item" <> "data-value" =: tshow i
               <> dataText
          dataText
            | Just dt <- _dataText = "data-text" =: dt
            | _textOnly = "data-text" =: t
            | otherwise = mempty
      in elAttr "div" attrs $ do
          maybe blank ui_ _icon
          maybe blank ui_ _image
          maybe blank ui_ _flag
          text t
    getIndices :: Foldable f => f a -> [Int]
    getIndices vs = L.findIndices ((`elem` vs) . _value) items

{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE Rank2Types               #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeFamilies             #-}

module Reflex.Dom.SemanticUI.RadioGroup
  (
  -- * Radio Group
    radioGroup
  , RadioGroupConfig (..)
  -- * Radio Item
  , RadioItem (..)
  , RadioItemConfig (..)

  ) where

import           Control.Monad (void)
import           Control.Monad.Trans
import           Control.Lens ((^.))
import           Data.Default
import qualified Data.List as L
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (isJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified GHCJS.DOM.Element as DOM
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core hiding
  ()

import Reflex.Dom.SemanticUI.Checkbox (CheckboxType (..))
import Reflex.Dom.SemanticUI.Common

--------------------------------------------------------------------------------
-- Radio Item

data RadioItemConfig = RadioItemConfig
  { _attributes :: Map Text Text
  -- ^ Attributes of the \<input\> element
  , _divAttributes :: Map Text Text
  -- ^ Attributes of the wrapping \<div\> element
  }

instance Default RadioItemConfig where
  def = RadioItemConfig
    { _attributes = mempty
    , _divAttributes = mempty
    }

data RadioItem t m a = RadioItem
  { _value :: a
  -- ^ Item value
  , _label :: m ()
  -- ^ Widget inside the \<label\> element
  , _config :: RadioItemConfig
  -- ^ Item config
  }

--------------------------------------------------------------------------------
-- Radio Group Config

data RadioGroupConfig t m a = RadioGroupConfig
  { _initialValue :: Maybe a
  -- ^ Initial value of the radio group. 'Nothing' means no items are selected.
  , _setValue :: Event t (Maybe a)
  -- ^ Event which sets the value. 'Nothing' clears the selection.
  , _types :: [CheckboxType]
  -- ^ Checkbox type (e.g. slider)
  , _wrapper :: m DOM.Element -> m DOM.Element
  --, _wrapper :: forall b. m b -> m b
  -- ^ Wrapper around each individual item
  }

instance MonadWidget t m => Default (RadioGroupConfig t m a) where
  def = RadioGroupConfig
    { _initialValue = Nothing
    , _setValue = never
    , _types = mempty
    , _wrapper = divClass "field"
    }

--------------------------------------------------------------------------------
-- Radio Group Functions

-- | Create a group of radio checkboxes from the given list of items. The name
-- is required to link the individual checkboxes together, it must be unique to
-- the field. The \<input\> elements are given values automatically by their
-- index.
--
-- https://semantic-ui.com/modules/checkbox.html#radio
radioGroup
  :: (Eq a, MonadWidget t m)
  => Text                   -- ^ Name of \<input\> elements
  -> [RadioItem t m a]      -- ^ Items
  -> RadioGroupConfig t m a -- ^ Group config
  -> m (Dynamic t (Maybe a))
radioGroup name items config = divClass "grouped fields" $ do

  -- Insert all of the items, collecting the raw elements and wrapping them with
  -- the given wrapper function
  inputEls <- traverse wrapper . imap (putRadioItem name classes) $ items

  -- Helper to lookup the index of an item
  let getIndex v = L.findIndex ((==) v . _value) items
      setRadio = liftJSM . setRadioGroup inputEls . (getIndex =<<)

  -- Event performed when user fires a set value event
  onSetEvent <- performEvent $ setRadio <$> _setValue config

  -- Set initial value
  pb <- getPostBuild
  setInitialEvent <- performEvent $ setRadio initialValue <$ pb

  -- On change callbacks
  (onChangeEvent, onChangeCallback) <- newTriggerEvent
  let setupCallbacks = liftJSM . setRadioCallbacks inputEls
                     $ liftIO . onChangeCallback
  performEvent_ $ setupCallbacks <$ pb

  index <- holdDyn Nothing $ leftmost [onChangeEvent, onSetEvent, setInitialEvent]
  return $ (\i -> fmap _value $ (items !?) =<< i) <$> index

  where
    initialValue = _initialValue config
    wrapper = _wrapper config
    cbType = _types config
    hasRadio = if isToggleOrSlider cbType then [] else ["radio"]
    classes = T.unwords $ "ui checkbox" : hasRadio ++ map uiText cbType
    -- Detect clashing classes: toggle and slider take precedence over radio
    isToggleOrSlider types = isJust $
      L.find (\i -> i == CbToggle || i == CbSlider) types

-- | Make an individual radio checkbox item.
putRadioItem
  :: MonadWidget t m
  => Text                 -- ^ HTML name attribute
  -> Text                 -- ^ Classes for the enclosing \<div\> element
  -> Int                  -- ^ Value of item
  -> RadioItem t m a  -- ^ Item configuration
  -> m DOM.Element
putRadioItem name classes i (RadioItem _ label (RadioItemConfig attrs' divAttrs')) = do

  -- Make the radio item
  (cbEl, inputEl) <- elAttr' "div" divAttrs $ do
    (inputEl, _) <- elAttr' "input" attrs blank
    void $ el "label" label
    return inputEl

  -- Setup radio checkbox element with semantic ui
  pb <- getPostBuild
  performEvent_ $ (liftJSM $ activateRadio $ _element_raw cbEl) <$ pb

  return $ _element_raw inputEl

  where
    attrs = "type" =: "radio" <> "value" =: tshow i <> "name" =: name <> attrs'
    divAttrs = M.alter alterClasses "class" divAttrs'
    alterClasses = maybe (Just classes) (\c -> Just $ T.unwords [classes, c])

--------------------------------------------------------------------------------
-- Javascript Functions

-- | Activate a radio element with Semantic UI. No callbacks by semantic ui
-- because they don't notify when a radio is automatically de-selected, so
-- instead we manually put on change listeners to the individual radio items.
activateRadio :: DOM.Element -> JSM ()
activateRadio e = void $ jQuery e ^. js0 ("checkbox" :: Text)

-- | Given a list of radio checkboxes, setup onChange callbacks
setRadioCallbacks :: [DOM.Element] -> (Maybe Int -> JSM ()) -> JSM ()
setRadioCallbacks es onChange = do
  let checked = jQuery es
        ^. js1 ("filter" :: Text) (":checked" :: Text)
        ^. js0 ("val" :: Text)
  let callback = fun $ \_ _ _ -> onChange =<< fromJSValUnchecked =<< checked
  void $ jQuery es ^. js2 ("on" :: Text) ("change" :: Text) callback

-- | Set the current value of a radio group.
setRadioGroup :: [DOM.Element] -> Maybe Int -> JSM (Maybe Int)
setRadioGroup es Nothing
  = Nothing <$ jQuery es ^. js2 ("prop" :: Text) ("checked" :: Text) False
setRadioGroup es (Just v) = do
  void $ jQuery es
    ^. js1 ("filter" :: Text) ("[value=" <> tshow v <> "]")
    ^. js2 ("prop" :: Text) ("checked" :: Text) True
  -- Try to prevent state being out of sync by returning which is selected
  selected <- jQuery es
    ^. js1 ("filter" :: Text) (":checked" :: Text)
    ^. js0 ("val" :: Text)
  syncPoint -- needed for the initial set event to fire
  fromJSValUnchecked selected

{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE Rank2Types               #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeFamilies             #-}

module Reflex.Dom.SemanticUI.RadioGroup where

------------------------------------------------------------------------------
import           Control.Monad (void)
import           Control.Monad.Trans
import           Control.Lens (makeLenses, (^.))
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
import           Reflex.Dom.Core hiding (fromJSString)
------------------------------------------------------------------------------
import           Reflex.Dom.SemanticUI.Checkbox
import           Reflex.Dom.SemanticUI.Common
------------------------------------------------------------------------------


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

------------------------------------------------------------------------------

-- | Holds the config of individual radio items
data RadioItemConfig t m = RadioItemConfig
  { _radioItemConfig_label :: Dynamic t (m ()) -- ^ label rendering
  , _radioItemConfig_attributes :: Map Text Text
  , _radioItemConfig_divAttributes :: Map Text Text
  }

$(makeLenses ''RadioItemConfig)

instance HasSetValue (RadioItemConfig t m) where
  type SetValue (RadioItemConfig t m) = Dynamic t (m ())
  setValue = radioItemConfig_label

instance HasAttributes (RadioItemConfig t m) where
  type Attrs (RadioItemConfig t m) = Map Text Text
  attributes = radioItemConfig_attributes

instance MonadWidget t m => Default (RadioItemConfig t m) where
  def = RadioItemConfig
    { _radioItemConfig_label = constDyn blank
    , _radioItemConfig_attributes = mempty
    , _radioItemConfig_divAttributes = mempty
    }

data RadioGroupConfig t m a = RadioGroupConfig
  { _radioGroupConfig_initialValue :: Maybe a
  , _radioGroupConfig_setValue :: Event t (Maybe a)
  , _radioGroupConfig_type :: [CheckboxType]
  , _radioGroupConfig_wrapper :: forall b. m b -> m b
  }

$(makeLenses ''RadioGroupConfig)

instance HasSetValue (RadioGroupConfig t m a) where
  type SetValue (RadioGroupConfig t m a) = Event t (Maybe a)
  setValue = radioGroupConfig_setValue

instance MonadWidget t m => Default (RadioGroupConfig t m a) where
  def = RadioGroupConfig
    { _radioGroupConfig_initialValue = Nothing
    , _radioGroupConfig_setValue = never
    , _radioGroupConfig_type = mempty
    , _radioGroupConfig_wrapper = divClass "field"
    }

-- | Create a group of radio checkboxes from the given list of items. The name
-- is required to link the individual checkboxes together. It must be unique to
-- the field.
radioGroup
  :: (Eq a, MonadWidget t m)
  => Text                       -- ^ Name
  -> [(a, RadioItemConfig t m)] -- ^ Item tuples: (selected type, item config)
  -> RadioGroupConfig t m a     -- ^ Config
  -> m (Dynamic t (Maybe a))
radioGroup name items config = do

  -- Insert all of the items, collecting the raw elements and wrapping them with
  -- the given wrapper function
  inputEls <- traverse wrapper . imap (putRadioItem name classes)
    $ map snd items

  -- Helper to lookup the index of an item
  let getIndex v = L.findIndex ((==) v . fst) items
      setRadio = liftJSM . setRadioGroup inputEls . (getIndex =<<)

  -- Event performed when user fires a set value event
  onSetEvent <- performEvent $ setRadio <$> _radioGroupConfig_setValue config

  -- Set initial value
  pb <- getPostBuild
  setInitialEvent <- performEvent $ setRadio initialValue <$ pb

  -- On change callbacks
  (onChangeEvent, onChangeCallback) <- newTriggerEvent
  let setupCallbacks = liftJSM . setRadioCallbacks inputEls
                     $ liftIO . onChangeCallback
  performEvent_ $ setupCallbacks <$ pb

  index <- holdDyn Nothing $ leftmost [onChangeEvent, onSetEvent, setInitialEvent]
  return $ (\i -> fmap fst $ (items !?) =<< i) <$> index

  where
    initialValue = _radioGroupConfig_initialValue config
    wrapper = _radioGroupConfig_wrapper config
    cbType = _radioGroupConfig_type config
    hasRadio = if isToggleOrSlider cbType then [] else ["radio"]
    classes = T.unlines $ "ui checkbox" : hasRadio ++ map uiText cbType
    -- Detect clashing classes: toggle and slider take precedence over radio
    isToggleOrSlider types = isJust $
      L.find (\i -> i == CbToggle || i == CbSlider) types

-- | Make an individual radio checkbox item.
putRadioItem
  :: MonadWidget t m
  => Text                 -- ^ HTML name attribute
  -> Text                 -- ^ Classes for the enclosing div element
  -> Int                  -- ^ Value of item
  -> RadioItemConfig t m  -- ^ Item configuration
  -> m DOM.Element
putRadioItem name classes i (RadioItemConfig label attrs' divAttrs') = do

  -- Make the radio item
  (cbEl, inputEl) <- elAttr' "div" divAttrs $ do
    (inputEl, _) <- elAttr' "input" attrs blank
    void $ el "label" $ dyn label
    return inputEl

  -- Setup radio checkbox element with semantic ui
  pb <- getPostBuild
  performEvent_ $ (liftJSM $ activateRadio $ _element_raw cbEl) <$ pb

  return $ _element_raw inputEl

  where
    attrs = "type" =: "radio" <> "value" =: tshow i <> "name" =: name <> attrs'
    divAttrs = M.alter alterClasses "class" divAttrs'
    alterClasses = maybe (Just classes) (\c -> Just $ T.unlines [classes, c])


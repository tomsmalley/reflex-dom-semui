{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE Rank2Types               #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeFamilies             #-}

module Reflex.Dom.SemanticUI.Checkbox where

------------------------------------------------------------------------------
import           Control.Monad (void)
import           Control.Monad.Trans
import           Control.Lens ((^.), makeLenses)
import           Data.Default
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified GHCJS.DOM.Element as DOM
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core hiding (fromJSString, checkbox)
------------------------------------------------------------------------------
import           Reflex.Dom.SemanticUI.Common (jQuery, UiClassText(..))
------------------------------------------------------------------------------

-- | SemanticUI javascript checkbox function
checkbox :: ToJSVal a => a -> JSF
checkbox = js1 ("checkbox" :: Text)

-- | Given a div element, tell semantic-ui to convert it to a checkbox with the
-- given options. The callback function is called on change with the new state.
activateCheckbox :: DOM.Element -> (Bool -> JSM ()) -> JSM ()
activateCheckbox e onChange = do
  o <- obj
  o <# ("onChange" :: Text) $ fun $ \ _ _ _ -> do
    state <- jQuery e ^. checkbox ("is checked" :: Text)
    onChange =<< fromJSValUnchecked state
  void $ jQuery e ^. checkbox o

-- | Given an initialised checkbox element, set the state to the given value.
-- The act of setting the state calls any callbacks, so the value stays
-- synchronised.
setCheckboxValue :: DOM.Element -> Bool -> JSM ()
setCheckboxValue e check = void $ jQuery e ^. checkbox arg
  where arg = if check then "check" else "uncheck" :: Text

--------------------------------------------------------------------------------

data CheckboxType =  CbSlider | CbToggle | CbFitted
  deriving Eq

-- | Convert an option to its class representation
instance UiClassText CheckboxType where
  uiText CbSlider = "slider"
  uiText CbToggle = "toggle"
  uiText CbFitted = "fitted"

data CheckboxConf t = CheckboxConf
  { _checkboxConf_initialValue :: Bool
  , _checkboxConf_setValue :: Event t Bool
  , _checkboxConf_attributes :: Map Text Text
  , _checkboxConf_divAttributes :: Map Text Text
  , _checkboxConf_type :: [CheckboxType]
  }

$(makeLenses ''CheckboxConf)

instance Reflex t => Default (CheckboxConf t) where
  def = CheckboxConf
    { _checkboxConf_initialValue = False
    , _checkboxConf_setValue = never
    , _checkboxConf_attributes = mempty
    , _checkboxConf_divAttributes = mempty
    , _checkboxConf_type = mempty
    }

instance HasAttributes (CheckboxConf t) where
  type Attrs (CheckboxConf t) = Map Text Text
  attributes = checkboxConf_attributes

instance HasSetValue (CheckboxConf t) where
  type SetValue (CheckboxConf t) = Event t Bool
  setValue = checkboxConf_setValue

--------------------------------------------------------------------------------

-- | Semantic UI checkbox, returning reflex-dom 'Checkbox' contents
uiCheckbox
  :: MonadWidget t m
  => m ()           -- ^ Label contents
  -> CheckboxConf t -- ^ Checkbox config
  -> m (Checkbox t)
uiCheckbox label config = fmap snd $ uiCheckbox' label config

-- | Semantic UI checkbox, returning element and reflex-dom 'Checkbox' contents
uiCheckbox'
  :: MonadWidget t m
  => m ()           -- ^ Label contents
  -> CheckboxConf t -- ^ Checkbox config
  -> m (El t, Checkbox t)
uiCheckbox' label config = do

  (cbEl, _) <- elAttr' "div" divAttrs $ do
    elAttr "input" attrs blank
    el "label" label

  -- Setup the event and callback function for when the value is changed
  (onChangeEvent, onChangeCallback) <- newTriggerEvent
  let setCbVal = setCheckboxValue (_element_raw cbEl)

  -- Activate the dropdown after build and set initial value
  schedulePostBuild $ liftJSM $ do
    activateCheckbox (_element_raw cbEl) $ liftIO . onChangeCallback
    setCbVal $ _checkboxConf_initialValue config


  -- Allow the value to be set
  performEvent_ $ liftJSM . setCbVal <$> _checkboxConf_setValue config

  cb <- holdDyn (M.member "checked" attrs) onChangeEvent
  return (cbEl, Checkbox cb onChangeEvent)

  where
    attrs = M.insert "type" "checkbox" $ _checkboxConf_attributes config
    classes = T.unlines $ "ui checkbox" : map uiText (_checkboxConf_type config)
    alterClasses = maybe (Just classes) (\c -> Just $ T.unlines [classes, c])
    divAttrs = M.alter alterClasses "class" $ _checkboxConf_divAttributes config


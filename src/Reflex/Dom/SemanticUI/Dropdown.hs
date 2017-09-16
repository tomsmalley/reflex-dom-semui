{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE RecursiveDo              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE TemplateHaskell          #-}

module Reflex.Dom.SemanticUI.Dropdown where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Trans
import           Control.Lens ((^.), makeLenses)
import           Data.Default
import qualified Data.List as L
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (catMaybes, maybeToList)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified GHCJS.DOM.Element as DOM
import           Language.Javascript.JSaddle
import           Text.Read (readMaybe)
import           Reflex
--import           Reflex.Host.Class
import           Reflex.Dom.Core hiding (fromJSString)
------------------------------------------------------------------------------
import           Reflex.Dom.SemanticUI.Common
------------------------------------------------------------------------------

-- | Custom Dropdown item configuration
data DropdownItemConfig m = DropdownItemConfig
  { dropdownItemConfig_dataText :: T.Text
    -- ^ dataText (shown for the selected item)
  , dropdownItemConfig_internal :: m ()
    -- ^ Procedure for drawing the DOM contents of the menu item
    --   (we produce the menu item div for you, so it's enough to
    --    use something simple here, e.g. `text "Friends"`
  }

-- | Dropdowns make have these additional properties
data DropdownOptFlag =
    DOFFluid
     -- ^ More flexible dropdown width, won't line break items
  | DOFSearch
    -- ^ Make menu items are searchable
  | DOFSelection
    -- ^ Dropdown is a selection among alternatives
  deriving (Eq, Enum, Show)

-- Helper function to build class attribute for dropdown
dropdownClass :: [DropdownOptFlag] -> T.Text
dropdownClass opts = T.unwords $ "ui" : (flags ++ ["dropdown"])
  where flags = map (T.toLower . T.drop 3 . tshow) $ L.sortOn fromEnum opts

------------------------------------------------------------------------------


-- | Given a div element, tell semantic-ui to convert it to a dropdown with the
-- given options. The callback function is called on change with the currently
-- selected value.
activateDropdown :: DOM.Element -> Maybe Int -> Bool -> Bool
                 -> (Text -> JSM ()) -> JSM ()
activateDropdown e maxSel useLabels fullText onChange = do
  o <- obj
  o <# ("forceSelection" :: Text) $ False
  o <# ("maxSelections" :: Text) $ maxSel
  o <# ("useLabels" :: Text) $ useLabels
  o <# ("fullTextSearch" :: Text) $ fullText
  o <# ("onChange" :: Text) $ fun $ \_ _ [t, _, _] ->
    onChange =<< fromJSValUnchecked t
  void $ jQuery e ^. js1 ("dropdown" :: Text) o

-- | Given a dropdown element, set the value to the given list. For single
-- dropdowns just provide a singleton list.
dropdownSetExactly :: DOM.Element -> [Int] -> JSM ()
dropdownSetExactly e is
  = void $ jQuery e ^. js2 ("dropdown" :: Text) ("set exactly" :: Text) is

------------------------------------------------------------------------------

-- | Config for new semantic dropdowns
data DropdownConf t a = DropdownConf
  { _dropdownConf_initialValue :: a
  , _dropdownConf_setValue :: Event t a
  , _dropdownConf_attributes :: Map Text Text
  , _dropdownConf_placeholder :: Text
  , _dropdownConf_maxSelections :: Maybe Int
  , _dropdownConf_useLabels :: Bool
  , _dropdownConf_fullTextSearch :: Bool
  } deriving Functor

$(makeLenses ''DropdownConf)

instance (Reflex t) => Default (DropdownConf t (Maybe a)) where
  def = DropdownConf
    { _dropdownConf_initialValue = Nothing
    , _dropdownConf_setValue = never
    , _dropdownConf_attributes = mempty
    , _dropdownConf_placeholder = mempty
    , _dropdownConf_maxSelections = Nothing
    , _dropdownConf_useLabels = True
    , _dropdownConf_fullTextSearch = False
    }

instance (Reflex t) => Default (DropdownConf t [a]) where
  def = DropdownConf
    { _dropdownConf_initialValue = mempty
    , _dropdownConf_setValue = never
    , _dropdownConf_attributes = mempty
    , _dropdownConf_placeholder = mempty
    , _dropdownConf_maxSelections = Nothing
    , _dropdownConf_useLabels = True
    , _dropdownConf_fullTextSearch = False
    }

instance HasAttributes (DropdownConf t a) where
  type Attrs (DropdownConf t a) = Map Text Text
  attributes = dropdownConf_attributes

instance HasSetValue (DropdownConf t a) where
  type SetValue (DropdownConf t a) = Event t a
  setValue = dropdownConf_setValue

-- | Helper function
indexToItem :: [(a, DropdownItemConfig m)] -> Text -> Maybe a
indexToItem items i' = do
  i <- readMaybe $ T.unpack i'
  fst <$> items !? i

-- | Semantic-UI dropdown with static items
uiDropdown
  :: (MonadWidget t m, Eq a)
  => [(a, DropdownItemConfig m)]  -- ^ Items
  -> [DropdownOptFlag]            -- ^ Options
  -> DropdownConf t (Maybe a)     -- ^ Dropdown config
  -> m (Dynamic t (Maybe a))
uiDropdown items options config = do
  (divEl, evt) <- dropdownInternal items options False (void config)
  let getIndex v = L.findIndex ((==) v . fst) items
      setDropdown = dropdownSetExactly (_element_raw divEl)

  -- setValue events
  performEvent_ $ liftJSM . setDropdown . maybeToList . (>>= getIndex)
               <$> _dropdownConf_setValue config

  -- Set initial value
  pb <- getPostBuild
  performEvent $
    liftJSM (setDropdown $ maybeToList $ getIndex
      =<< _dropdownConf_initialValue config) <$ pb

  holdDyn Nothing $ indexToItem items <$> evt

-- | Semantic-UI dropdown with multiple static items
uiDropdownMulti
  :: (MonadWidget t m, Eq a)
  => [(a, DropdownItemConfig m)]  -- ^ Items
  -> [DropdownOptFlag]            -- ^ Options
  -> DropdownConf t [a]           -- ^ Dropdown config
  -> m (Dynamic t [a])
uiDropdownMulti items options config = do
  (divEl, evt) <- dropdownInternal items options True (void config)

  let getIndices vs = L.findIndices ((`elem` vs) . fst) items
      setDropdown = dropdownSetExactly (_element_raw divEl)

  -- setValue events
  performEvent_ $ liftJSM . setDropdown . getIndices
               <$> _dropdownConf_setValue config

  -- Set initial value
  pb <- getPostBuild
  performEvent $
    liftJSM (setDropdown . getIndices $
      _dropdownConf_initialValue config) <$ pb

  holdDyn [] $ catMaybes . map (indexToItem items) . T.splitOn "," <$> evt

-- | Internal function with shared behaviour
dropdownInternal
  :: (MonadWidget t m, Eq a)
  => [(a, DropdownItemConfig m)]  -- ^ Items
  -> [DropdownOptFlag]            -- ^ Options
  -> Bool                         -- ^ Is multiple dropdown
  -> DropdownConf t ()            -- ^ Dropdown config
  -> m (El t, Event t Text)
dropdownInternal items options isMulti config = do

  (divEl, _) <- elAttr' "div" ("class" =: classes <> attrs) $ do

    -- This holds the placeholder. Initial value must be set by js function in
    -- wrapper.
    divClass "default text" $ text $ placeholder
    elAttr "i" ("class" =: "dropdown icon") blank

    -- Dropdown menu
    divClass "menu" $ sequence_ $ imap putItem items

  -- Setup the event and callback function for when the value is changed
  (onChangeEvent, onChangeCallback) <- newTriggerEvent

  -- Activate the dropdown after element is built
  schedulePostBuild $ liftJSM $
    activateDropdown (_element_raw divEl) maxSel useLabels fullText $
      liftIO . onChangeCallback

  return (divEl, onChangeEvent)

  where
    useLabels = _dropdownConf_useLabels config
    fullText = _dropdownConf_fullTextSearch config
    placeholder = _dropdownConf_placeholder config
    attrs = _dropdownConf_attributes config
    maxSel = if isMulti then _dropdownConf_maxSelections config
                        else Nothing
    multiClass = if isMulti then " multiple" else ""
    classes = dropdownClass options <> multiClass
    itemDiv i a = elAttr "div"
      ("class" =: "item" <> "data-value" =: tshow i <> a)
    putItem i (_, conf) = case conf of
      DropdownItemConfig "" m -> itemDiv i mempty m
      DropdownItemConfig t m -> itemDiv i ("data-text" =: t) m


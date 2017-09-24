{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE UndecidableInstances     #-}

module Reflex.Dom.SemanticUI.Dropdown
  (
    Dropdown (..)
  , uiDropdown
  , uiDropdownMulti
  , DropdownConfig (..)
  , DropdownItemConfig' (..)
  , DropdownOptFlag (..)
  , DropdownAction (..)

  ) where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Trans
import           Control.Lens ((^.))
import           Data.Default
import qualified Data.List as L
import           Data.Map (Map)
import           Data.Maybe (catMaybes, maybeToList)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified GHCJS.DOM.Element as DOM
import           Language.Javascript.JSaddle
import           Text.Read (readMaybe)
import           Reflex
--import           Reflex.Host.Class
import           Reflex.Dom.Core hiding
  ( Dropdown (..), DropdownConfig (..), Select )
------------------------------------------------------------------------------
import           Reflex.Dom.SemanticUI.Common
------------------------------------------------------------------------------

-- | Custom Dropdown item configuration
data DropdownItemConfig' m = DropdownItemConfig'
  { dataText :: T.Text
    -- ^ dataText (shown for the selected item)
  , internal :: m ()
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
  , _searchable :: Bool
  , _fluid :: Bool
  , _action :: DropdownAction
  , _item :: Bool
  } deriving Functor

instance (Reflex t) => Default (DropdownConfig t (Maybe a)) where
  def = DropdownConfig
    { _initialValue = Nothing
    , _setValue = never
    , _attributes = mempty
    , _placeholder = mempty
    , _maxSelections = Nothing
    , _useLabels = True
    , _fullTextSearch = False
    , _searchable = False
    , _fluid = False
    , _action = Activate
    , _item = False
    }

instance (Reflex t) => Default (DropdownConfig t [a]) where
  def = DropdownConfig
    { _initialValue = mempty
    , _setValue = never
    , _attributes = mempty
    , _placeholder = mempty
    , _maxSelections = Nothing
    , _useLabels = True
    , _fullTextSearch = False
    , _searchable = False
    , _fluid = False
    , _action = Activate
    , _item = False
    }

-- | Helper function
indexToItem :: [DropdownItem a] -> Text -> Maybe a
indexToItem items i' = do
  i <- readMaybe $ T.unpack i'
  _value <$> items !? i

-- | Helper function
indexToItem' :: [(a, DropdownItemConfig' m)] -> Text -> Maybe a
indexToItem' items i' = do
  i <- readMaybe $ T.unpack i'
  fst <$> items !? i

-- | Custom Dropdown item configuration
data DropdownItemConfig = DropdownItemConfig
--  { dataText :: T.Text
--    -- ^ dataText (shown for the selected item)
--  , _
--  }

data DropdownItem a = DropdownItem
  { _value :: a
  , _label :: Text
  , _config :: DropdownItemConfig
  }

data Dropdown t a = Dropdown
  { _items :: [DropdownItem a]
  , _config :: DropdownConfig t (Maybe a)
  }

data DropdownMulti t a = DropdownMulti
  { _items :: [DropdownItem a]
  , _config :: DropdownConfig t [a]
  }

instance (t ~ t', Eq a) => UI t' m (Dropdown t a) where
  type Return t' m (Dropdown t a) = Dynamic t (Maybe a)
  ui (Dropdown items config) = do
    --(divEl, evt) <- dropdownInternal (undefined :: [(a, DropdownItemConfig' m)]) [] False (void config)
    (divEl, evt) <- dropdownInternal (undefined :: [(a, DropdownItemConfig' m)]) [] False (void config)

    let setDropdown = liftJSM . dropdownSetExactly (_element_raw divEl)
                    . getIndices

    -- setValue events
    performEvent_ $ setDropdown <$> _setValue config

    -- Set initial value
    pb <- getPostBuild
    performEvent_ $ setDropdown (_initialValue config) <$ pb

    holdDyn Nothing $ indexToItem items <$> evt

    where
      getIndices :: Foldable f => f a -> [Int]
      getIndices vs = L.findIndices ((`elem` vs) . _value) items

-- | Semantic-UI dropdown with static items
uiDropdown
  :: (MonadWidget t m, Eq a)
  => [(a, DropdownItemConfig' m)]  -- ^ Items
  -> [DropdownOptFlag]            -- ^ Options
  -> DropdownConfig t (Maybe a)     -- ^ Dropdown config
  -> m (Dynamic t (Maybe a))
uiDropdown items options config = do
  (divEl, evt) <- dropdownInternal items options False (void config)

  let setDropdown = liftJSM . dropdownSetExactly (_element_raw divEl)
                  . maybeToList . (getIndex =<<)

  -- setValue events
  performEvent_ $ setDropdown <$> _setValue config

  -- Set initial value
  pb <- getPostBuild
  performEvent_ $ setDropdown initialValue <$ pb

  holdDyn Nothing $ indexToItem' items <$> evt

  where
    initialValue = _initialValue config
    getIndex v = L.findIndex ((==) v . fst) items

-- | Semantic-UI dropdown with multiple static items
uiDropdownMulti
  :: (MonadWidget t m, Eq a)
  => [(a, DropdownItemConfig' m)]  -- ^ Items
  -> [DropdownOptFlag]            -- ^ Options
  -> DropdownConfig t [a]           -- ^ Dropdown config
  -> m (Dynamic t [a])
uiDropdownMulti items options config = do
  (divEl, evt) <- dropdownInternal items options True (void config)

  let setDropdown = liftJSM . dropdownSetExactly (_element_raw divEl)
                  . getIndices

  -- setValue events
  performEvent_ $ setDropdown <$> _setValue config

  -- Set initial value
  pb <- getPostBuild
  performEvent_ $ setDropdown initialValue <$ pb

  holdDyn [] $ catMaybes . map (indexToItem' items) . T.splitOn "," <$> evt

  where
    initialValue = _initialValue config
    getIndices vs = L.findIndices ((`elem` vs) . fst) items

-- | Internal function with shared behaviour
dropdownInternal
  :: (MonadWidget t m, Eq a)
  => [(a, DropdownItemConfig' m)]  -- ^ Items
  -> [DropdownOptFlag]            -- ^ Options
  -> Bool                         -- ^ Is multiple dropdown
  -> DropdownConfig t ()            -- ^ Dropdown config
  -> m (El t, Event t Text)
dropdownInternal items options isMulti config = do

  (divEl, _) <- elAttr' "div" ("class" =: classes <> attrs) $ do

    -- This holds the placeholder. Initial value must be set by js function in
    -- wrapper.
    if action == Activate
    then divClass "default text" $ text placeholder
    else text placeholder -- No wrapper if the text doesn't get replaced by the action
    elAttr "i" ("class" =: "dropdown icon") blank

    -- Dropdown menu
    divClass "menu" $ sequence_ $ imap putItem items

  -- Setup the event and callback function for when the value is changed
  (onChangeEvent, onChangeCallback) <- newTriggerEvent

  -- Activate the dropdown after element is built
  let activate = activateDropdown (_element_raw divEl) maxSel useLabels fullText action
               $ liftIO . onChangeCallback
  pb <- getPostBuild
  performEvent_ $ liftJSM activate <$ pb

  return (divEl, onChangeEvent)

  where
    action = _action config
    useLabels = _useLabels config
    fullText = _fullTextSearch config
    placeholder = _placeholder config
    attrs = _attributes config
    maxSel = if isMulti then _maxSelections config
                        else Nothing
    multiClass = if isMulti then " multiple" else ""
    itemClass = if _item config then " item" else ""
    classes = dropdownClass options <> multiClass <> itemClass
    itemDiv i a = elAttr "div"
      ("class" =: "item" <> "data-value" =: tshow i <> a)
    putItem i (_, conf) = case conf of
      DropdownItemConfig' "" m -> itemDiv i mempty m
      DropdownItemConfig' t m -> itemDiv i ("data-text" =: t) m

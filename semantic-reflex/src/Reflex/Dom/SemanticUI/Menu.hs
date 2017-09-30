{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reflex.Dom.SemanticUI.Menu where

import Data.Kind (Type)
import           Control.Monad (void)
import           Data.Default (Default (def))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import Data.These
import           Reflex
import           Reflex.Dom.Core hiding (Dropdown(..), DropdownConfig(..), Link, value, Select)
import Data.Align

import Reflex.Dom.SemanticUI.Icon
import Reflex.Dom.SemanticUI.Label
import Reflex.Dom.SemanticUI.Common
import Reflex.Dom.SemanticUI.Dropdown

data MenuConfig t a = MenuConfig
  { _initialValue :: a
  , _setValue :: Event t a
  , _size :: Maybe Size
  , _vertical :: Bool
  , _fluid :: Bool
  , _textContent :: Bool
  , _customMenu :: Maybe Text
  , _floated :: Maybe Floated
  } deriving Functor

instance Reflex t => Applicative (MenuConfig t) where
  pure a = MenuConfig
    { _initialValue = a
    , _setValue = never
    , _size = Nothing
    , _vertical = False
    , _fluid = False
    , _textContent = False
    , _customMenu = Nothing
    , _floated = Nothing
    }
  MenuConfig { _initialValue = fInit, _setValue = fEvt }
    <*> mca@MenuConfig { _initialValue = aInit, _setValue = aEvt } = mca
      { _initialValue = fInit aInit
      , _setValue = fmapMaybe id
          $ fmap (these (const Nothing) (const Nothing) (\f a -> Just $ f a))
          $ align fEvt aEvt
      }

instance Reflex t => Default (MenuConfig t (Maybe a)) where
  def = pure Nothing

instance Reflex t => Default (MenuConfig t (Proxy a)) where
  def = pure Proxy


menuConfigClasses :: MenuConfig t a -> [Text]
menuConfigClasses MenuConfig {..} = catMaybes
--  [ justWhen _disabled "disabled"
--  , justWhen _loading "loading"
--  , justWhen _fitted "fitted"
--  , justWhen _link "link"
  [ uiText <$> _size
  , justWhen _vertical "vertical"
  , justWhen _fluid "fluid"
  , justWhen _textContent "text"
  , uiText <$> _floated
--  , uiText <$> _color
  , _customMenu
  ]

data Link
  = Link Text -- ^ A real link
  | StyleLink -- ^ A div formatted like a link
  | NoLink    -- ^ Not a link
  deriving (Eq, Show)

data MenuItemConfig t m = MenuItemConfig
  { _color :: Maybe Color
  , _link :: Link
  , _render :: Maybe (Dynamic t (m ())) -- Extra arbitrary content
  , _icon :: Maybe (Icon t)
  , _label :: Maybe (Label t)
  }

instance Default (MenuItemConfig t m) where
  def = MenuItemConfig
    { _color = Nothing
    , _link = NoLink
    , _render = Nothing
    , _icon = Nothing
    , _label = Nothing
    }

menuItemConfigClasses :: MenuItemConfig t m -> [Text]
menuItemConfigClasses MenuItemConfig {..} = catMaybes
  [ uiText <$> _color
  , justWhen (_link == StyleLink) "link"
  ]

data Proxy a = Proxy

data MenuItems t m a (xs :: [Type]) where
  -- | Empty menu
  MenuBase :: MenuItems t m a '[]
  -- | Normal clickable menu item
  MenuItem :: a -> Text -> MenuItemConfig t m -> MenuItems t m a xs -> MenuItems t m a xs
  -- | Sub widget, capturing the value
  MenuWidget :: m b -> MenuItemConfig t m -> MenuItems t m a xs -> MenuItems t m a (b ': xs)
  -- | Arbitrary widget, ignoring the value
  MenuWidget_ :: m b -> MenuItemConfig t m -> MenuItems t m a xs -> MenuItems t m a xs
  -- | Sub menu
  SubMenu :: HListAppend ys xs => Dynamic t (m ()) -> MenuItems t m a ys -> MenuItems t m a xs -> MenuItems t m a (ys `Append` xs)
  RightMenu :: HListAppend ys xs => MenuItems t m a ys -> MenuItems t m a xs -> MenuItems t m a (ys `Append` xs)
  -- | Dropdown menu
  DropdownMenu :: Text -> [DropdownItem t m a] -> MenuItems t m a xs -> MenuItems t m a xs

type family Append (as :: [Type]) (bs :: [Type]) :: [Type] where
  Append '[] bs = bs
  Append (a ': as) bs = a ': (Append as bs)

class HListAppend as bs where
  hlistAppend :: HList as -> HList bs -> HList (Append as bs)

instance HListAppend '[] bs where
  hlistAppend HNil bs = bs
instance (Append (a ': as) bs ~ (a ': Append as bs), HListAppend as bs) => HListAppend (a ': as) bs where
  hlistAppend (a `HCons` as) bs = a `HCons` hlistAppend as bs

{-

data HMenu t m a xs = HMenu
  { _items :: HList xs
  , _config :: MenuConfig t (Maybe a)
  }

instance UI t' m' (HMenu t m a xs) where
  type Return t' m' (HMenu t m a xs) = (Dynamic t (Maybe a), HList xs)
  ui = undefined

data MItem t m a b where
  MItem :: a -> m () -> MItem t m a b
  MSub :: m b -> MItem t m a b

renderHItems
  :: HList xs
  -> Dynamic t (Maybe a)
  -> m ([Event t a], HList xs)
renderHItems allItems currentValue = go allItems
  where
    selected = demux currentValue

    go :: HList ys -> m ([Event t a], HList ys)
    go = \case

-}

data Menu t m a xs = Menu
  { _items :: MenuItems t m a xs
  , _config :: MenuConfig t (Maybe a)
  }

instance (Ord a, m ~ m', t ~ t') => UI t' m' (Menu t m a xs) where
  type Return t' m' (Menu t m a xs) = (Dynamic t (Maybe a), HList xs)
  ui' (Menu items config@MenuConfig {..}) = elClass' "div" (T.unwords classes) $ do
    rec (evts, xs) <- renderItems items vDyn
        vDyn <- holdDyn _initialValue $ leftmost $ _setValue : (fmap Just <$> evts)
    return (vDyn, xs)
    where
      classes = "ui" : "menu" : menuConfigClasses config

data MenuDef t m a xs = MenuDef
  { _items :: MenuItems t m a xs
  , _config :: MenuConfig t a
  }

instance (Ord a, m ~ m', t ~ t') => UI t' m' (MenuDef t m a xs) where
  type Return t' m' (MenuDef t m a xs) = (Dynamic t a, HList xs)
  ui' (MenuDef items config@MenuConfig {..}) = elClass' "div" (T.unwords classes) $ do
    rec (evts, xs) <- renderItems items (Just <$> vDyn)
        vDyn <- holdDyn _initialValue $ leftmost $ _setValue : evts
    return (vDyn, xs)
    where
      classes = "ui" : "menu" : menuConfigClasses config

renderItems
  :: forall t m a xs. (Ord a, Reflex t, MonadWidget t m)
  => MenuItems t m a xs         -- ^ Menu items
  -> Dynamic t (Maybe a)        -- ^ The currently selected value
  -> m ([Event t a], HList xs)
  -- ^ (List of selection events with tagged value, list of captures)
renderItems allItems currentValue = go False allItems
  where
    selected = demux currentValue

    itemElAttrs :: MenuItemConfig t m -> (Text, Map Text Text)
    itemElAttrs conf@MenuItemConfig{..} = case _link of
      Link href -> ("a", "href" =: href <> "class" =: classes)
      _ -> ("div", "class" =: classes)
      where classes = T.unwords $ "item" : menuItemConfigClasses conf

    go :: Bool -> MenuItems t m a ys -> m ([Event t a], HList ys)
    go inDropdown = \case

      MenuBase -> return ([], HNil)

      MenuItem value label conf@MenuItemConfig {..} rest -> do
        clickEvt <- fmap (domEvent Click . fst) $ elDynAttr' elType attrs $ do
            maybe blank ui_ _icon
            text label
            maybe blank ui_ _label
        (evts, hlist) <- go inDropdown rest
        return ((value <$ clickEvt) : evts, hlist)
          where
            (elType, attrs') = itemElAttrs conf { _link = reLink }
            reLink = case _link of
              NoLink -> StyleLink
              a -> a
            attrs = fmap (addActive attrs') $ demuxed selected $ Just value
            addActive m isActive = M.adjust (<> if isActive then " active" else "") "class" m

      MenuWidget mb conf rest -> do
        b <- elAttr elType attrs mb
        fmap (HCons b) <$> go inDropdown rest
          where (elType, attrs) = itemElAttrs conf

      MenuWidget_ mb conf rest -> elAttr elType attrs mb >> go inDropdown rest
        where (elType, attrs) = itemElAttrs conf

      DropdownMenu label items rest -> do
        dynVal <- ui $ Dropdown items $ (def :: DropdownConfig t (Maybe a))
          { _item = True
          -- DropdownMenu does not show the state in the dropdown
          , _action = Hide
          , _placeholder = label
          }
        (restEvents, restList) <- go inDropdown rest
        return (fmapMaybe id (updated dynVal) : restEvents, restList)

      SubMenu mkItem sub rest -> do
        (subEvents, subList) <- divClass "item" $ do
          void $ dyn mkItem
          divClass (T.unwords classes) $ go inDropdown sub
        (restEvents, restList) <- go inDropdown rest
        return (restEvents ++ subEvents, subList `hlistAppend` restList)
          where classes = pure "menu" -- : menuConfigClasses config

      RightMenu sub rest -> do
        (subEvents, subList) <- divClass "right menu" $ go inDropdown sub
        (restEvents, restList) <- go inDropdown rest
        return (restEvents ++ subEvents, subList `hlistAppend` restList)


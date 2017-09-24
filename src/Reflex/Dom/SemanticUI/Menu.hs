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
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module Reflex.Dom.SemanticUI.Menu where

import GHC.TypeLits as TL
import Data.Kind (Type)
import           Control.Monad (void)
import           Control.Monad.Trans (liftIO)
import           Control.Lens ((^.))
import           Data.Default (Default (def))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified GHCJS.DOM.Element as DOM
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core

import Debug.Trace

import Reflex.Dom.SemanticUI.Icon
import Reflex.Dom.SemanticUI.Common

data MenuConfig t a = MenuConfig
  { _initialValue :: a
  , _setValue :: Event t a
  , _size :: Maybe Size
  , _vertical :: Bool
  , _customMenu :: Maybe Text
  }

initMenuConfig :: Reflex t => a -> MenuConfig t a
initMenuConfig a = MenuConfig
    { _setValue = never
    , _initialValue = a
    , _size = Nothing
    , _vertical = False
    , _customMenu = Nothing
    }

instance Reflex t => Default (MenuConfig t (Maybe a)) where
  def = MenuConfig
    { _setValue = never
    , _initialValue = Nothing
    , _size = Nothing
    , _vertical = False
    , _customMenu = Nothing
    }

instance Reflex t => Default (MenuConfig t (Proxy a)) where
  def = MenuConfig
    { _setValue = never
    , _initialValue = Proxy
    , _size = Nothing
    , _vertical = False
    , _customMenu = Nothing
    }


menuConfigClasses :: MenuConfig t a -> [Text]
menuConfigClasses MenuConfig {..} = catMaybes
--  [ justWhen _disabled "disabled"
--  , justWhen _loading "loading"
--  , justWhen _fitted "fitted"
--  , justWhen _link "link"
  [ uiText <$> _size
  , justWhen _vertical "vertical"
--  , uiText <$> _floated
--  , uiText <$> _color
  , _customMenu
  ]

data MenuItemConfig = MenuItemConfig
  { _color :: Maybe Color
  }

instance Default MenuItemConfig where
  def = MenuItemConfig
    { _color = Nothing
    }

menuItemConfigClasses :: MenuItemConfig -> [Text]
menuItemConfigClasses MenuItemConfig {..} = catMaybes
  [ uiText <$> _color
  ]

data Proxy a = Proxy

data MenuItems t m a (xs :: [Type]) where
  -- | Empty menu
  MenuBase :: MenuItems t m a '[]
  -- | Normal clickable menu item
  MenuItem :: a -> Dynamic t (m ()) -> MenuItemConfig -> MenuItems t m a xs -> MenuItems t m a xs
  -- | Sub widget, capturing the value
  MenuUICapture :: (Return t m b ~ rb, UI t m b) => b -> MenuItemConfig -> MenuItems t m a xs -> MenuItems t m a (rb ': xs)
  -- | Sub widget, capturing the value
  MenuCapture :: m b -> MenuItems t m a xs -> MenuItems t m a (b ': xs)
  -- | Sub widget, ignoring the value
  MenuIgnore :: m b -> MenuItems t m a xs -> MenuItems t m a xs
  -- | Sub menu
  MenuSub :: HListAppend xs ys => MenuConfig t (Proxy a) -> MenuItems t m a ys -> MenuItems t m a xs -> MenuItems t m a (xs `Append` ys)

type family Append (as :: [Type]) (bs :: [Type]) :: [Type] where
  Append '[] bs = bs
  Append as '[] = as
  Append (a ': as) bs = a ': (Append as bs)

class HListAppend as bs where
  hlistAppend :: HList as -> HList bs -> HList (Append as bs)

instance HListAppend '[] '[] where
  hlistAppend HNil HNil = HNil
instance HListAppend '[] bs where
  hlistAppend HNil bs = bs
instance HListAppend as '[] where
  hlistAppend as HNil = as
instance (Append (a ': as) bs ~ (a ': Append as bs), HListAppend as bs) => HListAppend (a ': as) bs where
  hlistAppend (a `HCons` as) bs = a `HCons` hlistAppend as bs

data Menu t m a xs = Menu
  { _items :: MenuItems t m a xs
  , _config :: MenuConfig t (Maybe a)
  }

instance (Ord a, m ~ m', t ~ t') => UI t' m' (Menu t m a xs) where
  type Return t' m' (Menu t m a xs) = (Dynamic t (Maybe a), HList xs)
  ui (Menu items config@MenuConfig {..}) = divClass (T.unwords classes) $ do
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
  ui (MenuDef items config@MenuConfig {..}) = divClass (T.unwords classes) $ do
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
renderItems allItems currentValue = go allItems
  where
    selected = demux currentValue

    go :: MenuItems t m a ys -> m ([Event t a], HList ys)
    go = \case

      MenuBase -> return ([], HNil)

      MenuItem value mkItem conf rest -> do
        evtEl <- dyn $ elDynAttr' "a" classes <$> mkItem
        let clickEvt = domEvent Click . fst <$> evtEl
        clickEvt' <- switchPromptly never clickEvt
        (evts, hlist) <- go rest
        return ((value <$ clickEvt') : evts, hlist)
        where
          classes = fmap itemClasses $ demuxed selected $ Just value
          itemClasses isActive = M.singleton "class" $ T.unwords
            $ "item" : menuItemConfigClasses conf ++ if isActive then ["active"] else []

      MenuCapture mb rest -> do
        b <- mb
        fmap (HCons b) <$> go rest

      MenuIgnore mb rest -> mb >> go rest

      MenuSub config sub rest -> divClass (T.unwords classes) $ do
        (subEvents, subList) <- go sub
        (itemsEvents, itemsList) <- go rest
        return (itemsEvents ++ subEvents, itemsList `hlistAppend` subList)
          where classes = "menu" : menuConfigClasses config


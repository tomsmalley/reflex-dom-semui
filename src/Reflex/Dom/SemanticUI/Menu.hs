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

data MenuItem t m a
  = MenuItem a (Dynamic t (m ())) MenuItemConfig
  | forall b. MenuItemD (b -> a) (Dynamic t (m b)) MenuItemConfig

data Menu t m a = Menu
  { _items :: [MenuItem t m a]
  , _config :: MenuConfig t (Maybe a)
  }

instance (t' ~ t, m' ~ m, Eq a) => UI t' m' (Menu t m a) where
  type Return t' m' (Menu t m a) = Dynamic t (Maybe a)
  ui (Menu items config@MenuConfig {..}) = divClass (T.unwords classes) $ do
    rec evts <- traverse (putItem vDyn) items
        vDyn <- holdDyn _initialValue $ leftmost $ _setValue : evts
    return vDyn
    where
      classes = "ui" : "menu" : menuConfigClasses config
      putItem current (MenuItem value mkItem conf) = do
        evtEl <- dyn $ elDynAttr' "a" (itemClasses <$> current) <$> mkItem
        let clickEvt = domEvent Click . fst <$> evtEl
        clickEvt' <- switchPromptly never clickEvt
        return $ (Just value :: Maybe a) <$ clickEvt'
        where
          itemClasses mCurrent = M.singleton "class" $ T.unwords
                               $ "item" : menuItemConfigClasses conf ++ if mCurrent == Just value then ["active"] else []

data Proxy a = Proxy

data MItems t m a (xs :: [Type]) where
  -- | Empty menu
  MNil :: MItems t m a '[]
  -- | Normal clickable menu item
  MItem :: a -> Dynamic t (m ()) -> MenuItemConfig -> MItems t m a xs -> MItems t m a xs
  -- | Sub widget, capturing the value
  MCapture :: m b -> MItems t m a xs -> MItems t m a (b ': xs)
  -- | Sub widget, ignoring the value
  MIgnore :: m b -> MItems t m a xs -> MItems t m a xs
  -- | Sub menu
  MSubMenu :: HListAppend xs ys => MenuConfig t (Proxy a) -> MItems t m a ys -> MItems t m a xs -> MItems t m a (xs `Append` ys)

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

data MMenu t m a xs = MMenu
  { _items :: MItems t m a xs
  , _config :: MenuConfig t (Maybe a)
  }

instance (Ord a, m ~ m', t ~ t') => UI t' m' (MMenu t m a xs) where
  type Return t' m' (MMenu t m a xs) = (Dynamic t (Maybe a), HList xs)
  ui (MMenu items config@MenuConfig {..}) = divClass (T.unwords classes) $ do
    rec (evts, xs) <- renderItems items vDyn
        vDyn <- holdDyn _initialValue $ leftmost $ _setValue : (fmap Just <$> evts)
    return (vDyn, xs)
    where
      classes = "ui" : "menu" : menuConfigClasses config

data MenuDef t m a xs = MenuDef
  { _items :: MItems t m a xs
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

renderItems :: (Ord a, Reflex t, MonadWidget t m) => MItems t m a xs -> Dynamic t (Maybe a)
            -> m ([Event t a], HList xs)
renderItems allItems dynVal = let sel = demux dynVal in renderItems' allItems sel

renderItems' :: (Eq a, MonadWidget t m) => MItems t m a xs -> Demux t (Maybe a)
            -> m ([Event t a], HList xs)
renderItems' allItems selected = case allItems of

  MNil -> return ([], HNil)

  MItem value mkItem conf items -> do
    evtEl <- dyn $ elDynAttr' "a" classes <$> mkItem
    let clickEvt = domEvent Click . fst <$> evtEl
    clickEvt' <- switchPromptly never clickEvt
    (evts, hlist) <- renderItems' items selected
    return ((value <$ clickEvt') : evts, hlist)
    where
      classes = fmap itemClasses $ demuxed selected $ Just value
      itemClasses isActive = M.singleton "class" $ T.unwords
        $ "item" : menuItemConfigClasses conf ++ if isActive then ["active"] else []

  MCapture mb items -> do
    b <- mb
    fmap (HCons b) <$> renderItems' items selected

  MIgnore mb items -> mb >> renderItems' items selected

  MSubMenu config sub items -> divClass (T.unwords classes) $ do
    (subEvents, subList) <- renderItems' sub selected
    (itemsEvents, itemsList) <- renderItems' items selected
    return (itemsEvents ++ subEvents, itemsList `hlistAppend` subList)
      where classes = "menu" : menuConfigClasses config


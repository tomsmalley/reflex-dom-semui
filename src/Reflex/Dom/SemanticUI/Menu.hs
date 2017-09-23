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
  { _initialValue :: Maybe a
  , _setValue :: Event t (Maybe a)
  , _size :: Maybe Size
  , _vertical :: Bool
  }

instance Reflex t => Default (MenuConfig t a) where
  def = MenuConfig
    { _setValue = never
    , _initialValue = Nothing
    , _size = Nothing
    , _vertical = False
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
  , _config :: MenuConfig t a
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


data MItems t m a (l :: [Type]) where
  MNil :: MItems t m a '[]
  MItem :: a -> Dynamic t (m ()) -> MenuItemConfig -> MItems t m a l -> MItems t m a l
  MCapture :: m b -> MItems t m a l -> MItems t m a (b ': l)
  MIgnore :: m b -> MItems t m a l -> MItems t m a l

data MMenu t m a l = MMenu
  { _items :: MItems t m a '[] -> MItems t m a l
  , _config :: MenuConfig t a
  }

instance (Eq a, m ~ m', t ~ t') => UI t' m' (MMenu t m a l) where
  type Return t' m' (MMenu t m a l) = (Dynamic t (Maybe a), HList l)
  ui (MMenu items config@MenuConfig {..}) = divClass (T.unwords classes) $ do
    (evts, l) <- ui $ items MNil
    vDyn <- holdDyn _initialValue $ leftmost $ _setValue : (fmap Just <$> evts)
    return (vDyn, l)
    where
      classes = "ui" : "menu" : menuConfigClasses config

instance (Eq a, m ~ m', t ~ t') => UI t' m' (MItems t m a l) where
  type Return t' m' (MItems t m a l) = ([Event t a], HList l)
  ui MNil = return ([], HNil)
  ui (MItem value mkItem conf items) = do
    evtEl <- dyn $ elDynAttr' "a" (itemClasses <$> constDyn Nothing) <$> mkItem
    let clickEvt = domEvent Click . fst <$> evtEl
    clickEvt' <- switchPromptly never clickEvt
    (evts, hlist) <- ui items
    return ((value <$ clickEvt') : evts, hlist)
    where
      itemClasses mCurrent = M.singleton "class" $ T.unwords
        $ "item" : menuItemConfigClasses conf ++ if mCurrent == Just value then ["active"] else []
  ui (MCapture mb items) = do
    b <- mb
    fmap (HCons b) <$> ui items
  ui (MIgnore mb items) = mb >> ui items


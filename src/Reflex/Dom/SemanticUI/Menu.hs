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
{-# LANGUAGE DeriveFunctor #-}

module Reflex.Dom.SemanticUI.Menu where

import GHC.TypeLits as TL
import Data.Kind (Type)
import           Control.Monad (unless, void)
import           Control.Monad.Trans (liftIO)
import           Control.Lens ((^.))
import           Data.Default (Default (def))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import Data.These
import qualified GHCJS.DOM.Element as DOM
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core hiding (Link)
import Data.Align

import Debug.Trace

import Reflex.Dom.SemanticUI.Icon
import Reflex.Dom.SemanticUI.Common

data MenuConfig t a = MenuConfig
  { _initialValue :: a
  , _setValue :: Event t a
  , _size :: Maybe Size
  , _vertical :: Bool
  , _customMenu :: Maybe Text
  } deriving Functor

instance Reflex t => Applicative (MenuConfig t) where
  pure a = MenuConfig
    { _initialValue = a
    , _setValue = never
    , _size = Nothing
    , _vertical = False
    , _customMenu = Nothing
    }
  mcf <*> mca = mca
    { _initialValue = (_initialValue mcf) (_initialValue mca)
    , _setValue = fmapMaybe id
        $ fmap (these (const Nothing) (const Nothing) (\f a -> Just $ f a))
        $ align (_setValue mcf) (_setValue mca)
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

data Link
  = Link Text -- ^ A real link
  | StyleLink -- ^ A div formatted like a link
  | NoLink    -- ^ Not a link
  deriving (Eq, Show)

data MenuItemConfig = MenuItemConfig
  { _color :: Maybe Color
  , _link :: Link
  }

instance Default MenuItemConfig where
  def = MenuItemConfig
    { _color = Nothing
    , _link = NoLink
    }

menuItemConfigClasses :: MenuItemConfig -> [Text]
menuItemConfigClasses MenuItemConfig {..} = catMaybes
  [ uiText <$> _color
  , justWhen (_link == StyleLink) "link"
  ]

data Proxy a = Proxy

data MenuItems t m a (xs :: [Type]) where
  -- | Empty menu
  MenuBase :: MenuItems t m a '[]
  -- | Normal clickable menu item
  MenuItem :: a -> Dynamic t (m ()) -> MenuItemConfig -> MenuItems t m a xs -> MenuItems t m a xs
  -- | Sub menu
  SubMenu :: HListAppend ys xs => Dynamic t (m ()) -> MenuItems t m a ys -> MenuItems t m a xs -> MenuItems t m a (ys `Append` xs)
  -- | Sub widget, capturing the value
  UIContent :: (Return t m b ~ rb, Part t m b) => b -> MenuItemConfig -> MenuItems t m a xs -> MenuItems t m a (rb ': xs)
  -- | Sub widget, ignoring the value
  UIContent_ :: Part t m b => b -> MenuItemConfig -> MenuItems t m a xs -> MenuItems t m a xs
  -- | Sub item widget, capturing the value
  UIItemContent :: (Return t m b ~ rb, Item b, UI t m b) => b -> MenuItems t m a xs -> MenuItems t m a (rb ': xs)
  -- | Sub item widget, ignoring the value
  UIItemContent_ :: (Item b, UI t m b) => b -> MenuItems t m a xs -> MenuItems t m a xs
  -- | Sub widget, capturing the value
  MenuWidget :: m b -> MenuItemConfig -> MenuItems t m a xs -> MenuItems t m a (b ': xs)
  -- | Arbitrary widget, ignoring the value
  MenuWidget_ :: m b -> MenuItemConfig -> MenuItems t m a xs -> MenuItems t m a xs
  -- | Sub menu
  MenuSub :: HListAppend ys xs => MenuConfig t (Proxy a) -> MenuItems t m a ys -> MenuItems t m a xs -> MenuItems t m a (ys `Append` xs)
  -- | Dropdown item
  MenuDropdown :: HListAppend ys xs => Text -> MenuItems t m a ys -> MenuItems t m a xs -> MenuItems t m a (ys `Append` xs)

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
renderItems allItems currentValue = go False allItems
  where
    selected = demux currentValue

    itemElAttrs :: MenuItemConfig -> (Text, Map Text Text)
    itemElAttrs conf@MenuItemConfig{..} = case _link of
      Link href -> ("a", "href" =: href <> "class" =: classes)
      _ -> ("div", "class" =: classes)
      where classes = T.unwords $ "item" : menuItemConfigClasses conf

    go :: Bool -> MenuItems t m a ys -> m ([Event t a], HList ys)
    go inDropdown = \case

      MenuBase -> return ([], HNil)

      -- TODO: Icons in config, Dynamic element into config, just allow text or
      -- non dynamic rendering
      MenuItem value mkItem conf@MenuItemConfig {..} rest -> do
        evtEl <- dyn $ elDynAttr' elType attrs <$> mkItem
        let clickEvt = domEvent Click . fst <$> evtEl
        clickEvt' <- switchPromptly never clickEvt
        (evts, hlist) <- go inDropdown rest
        return ((value <$ clickEvt') : evts, hlist)
          where
            (elType, attrs') = itemElAttrs conf { _link = StyleLink }
            attrs = fmap (addActive attrs') $ demuxed selected $ Just value
            addActive m isActive = M.adjust (<> if isActive then " active" else "") "class" m

      MenuWidget mb conf rest -> do
        b <- elAttr elType attrs mb
        fmap (HCons b) <$> go inDropdown rest
          where (elType, attrs) = itemElAttrs conf

      MenuWidget_ mb conf rest -> elAttr elType attrs mb >> go inDropdown rest
        where (elType, attrs) = itemElAttrs conf

  --  UIContent :: (Return t m b ~ rb, UI t m b) => b -> MenuItemConfig -> MenuItems t m a xs -> MenuItems t m a (rb ': xs)
      UIContent uiElem conf rest -> do
        b <- elAttr elType attrs $ part uiElem
        (restEvents, restList) <- go inDropdown rest
        return (restEvents, b `HCons` restList)
          where (elType, attrs) = itemElAttrs conf

      UIContent_ uiElem conf rest -> do
        elAttr elType attrs $ part uiElem
        go inDropdown rest
          where (elType, attrs) = itemElAttrs conf

      UIItemContent uiElem rest -> do
        b <- ui uiElem
        (restEvents, restList) <- go inDropdown rest
        return (restEvents, b `HCons` restList)

      UIItemContent_ uiElem rest -> do
        ui $ toItem uiElem
        go inDropdown rest

      MenuDropdown label sub rest -> do
        (e, (subEvents, subList)) <- elClass' "div" (T.unwords classes) $ do
          ui $ Icon "dropdown" def -- icon must come first for sub dropdowns
          text label
          divClass "menu" $ go True sub
        -- Only do this on the top level dropdown!
        unless inDropdown $ void $ liftJSM $ jQuery (_element_raw e) ^. js0 ("dropdown" :: Text)
        (restEvents, restList) <- go inDropdown rest
        return (restEvents ++ subEvents, subList `hlistAppend` restList)
          where classes = if inDropdown
                          then ["item"]
                          else "ui" : "dropdown" : "item" : []

      SubMenu mkItem sub rest -> do
        (subEvents, subList) <- divClass "item" $ do
          dyn mkItem
          divClass (T.unwords classes) $ go inDropdown sub
        (restEvents, restList) <- go inDropdown rest
        return (restEvents ++ subEvents, subList `hlistAppend` restList)
          where classes = pure "menu" -- : menuConfigClasses config

      MenuSub config sub rest -> do
        (subEvents, subList) <- divClass (T.unwords classes) $ go inDropdown sub
        (restEvents, restList) <- go inDropdown rest
        return (restEvents ++ subEvents, subList `hlistAppend` restList)
          where classes = "menu" : menuConfigClasses config


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

module Reflex.Dom.SemanticUI.Menu where

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
import Reflex.Dom.SemanticUI.Common (jQuery, UiClassText(..), consoleLog, Size(..), UI (..), Floated(..)
  )

data MenuConfig a = MenuConfig
  { _initialValue :: Maybe a
  , _size :: Maybe Size
  }

instance Default (MenuConfig a) where
  def = MenuConfig
    { _size = Nothing
    , _initialValue = Nothing
    }

menuConfigClasses :: MenuConfig a -> [Text]
menuConfigClasses MenuConfig {..} = catMaybes
--  [ justWhen _disabled "disabled"
--  , justWhen _loading "loading"
--  , justWhen _fitted "fitted"
--  , justWhen _link "link"
--  , justWhen _inverted "inverted"
  [ uiText <$> _size
--  , uiText <$> _floated
--  , uiText <$> _color
  ]

data MenuItem t m a = MenuItem
  { _value :: a
  , _label :: Dynamic t (m ())
  }

data Menu t m a = Menu
  { _items :: [MenuItem t m a]
  , _config :: MenuConfig a
  }

--selectViewList :: a -> Dynamic t [a] -> (a -> Dynamic t Bool -> m (Event t a)) -> m (Dynamic t a)
--selectViewList initialValue dynItems mkChild = do
--  let selectionDemux = demux selection
--
--  selectChild <- flip traverse dynItems

instance (t' ~ t, m' ~ m, Eq a) => UI t' m' (Menu t m a) where
  type Return t' m' (Menu t m a) = Dynamic t (Maybe a)
  ui (Menu items config@MenuConfig {..}) = divClass (T.unwords classes) $ do
    rec evts <- traverse (putItem vDyn) items
        vDyn <- holdDyn _initialValue $ leftmost evts
    return vDyn
    where
      classes = "ui" : "menu" : menuConfigClasses config
      putItem current MenuItem {..} = do
        evtEl <- dyn $ elDynAttr' "a" (itemClasses <$> current) <$> _label
        let clickEvt = domEvent Click . fst <$> evtEl
        clickEvt' <- switchPromptly never clickEvt
        return $ (Just _value :: Maybe a) <$ clickEvt'
        where
          itemClasses mCurrent = M.singleton "class" $ T.unwords
                               $ "item" : if mCurrent == Just _value then ["active"] else []


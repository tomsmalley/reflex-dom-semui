{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Reflex.Dom.SemanticUI
  ( module Reflex.Dom.SemanticUI.Button
  , module Reflex.Dom.SemanticUI.Checkbox
  , module Reflex.Dom.SemanticUI.Common
  , module Reflex.Dom.SemanticUI.Dropdown
  , module Reflex.Dom.SemanticUI.Icon
  , module Reflex.Dom.SemanticUI.Input
  , module Reflex.Dom.SemanticUI.Header
  , module Reflex.Dom.SemanticUI.Lenses
  , module Reflex.Dom.SemanticUI.Modal
  , module Reflex.Dom.SemanticUI.RadioGroup
  , semanticCSS
  , semanticMain
  , semanticMainWithCss
  , module Reflex.Dom.Core
  ) where

------------------------------------------------------------------------------
import           Data.ByteString
import           Data.FileEmbed
import           Data.Text (Text)
------------------------------------------------------------------------------
import           Reflex.Dom.SemanticUI.Button
import           Reflex.Dom.SemanticUI.Checkbox
import           Reflex.Dom.SemanticUI.Common (basic, mini, compact, rightFloated, custom, tshow, Color (..), Size (..), small, ui, Floated(..))
import           Reflex.Dom.SemanticUI.Dropdown
import           Reflex.Dom.SemanticUI.Header
import           Reflex.Dom.SemanticUI.Icon
import           Reflex.Dom.SemanticUI.Input
import           Reflex.Dom.SemanticUI.Lenses
import           Reflex.Dom.SemanticUI.Modal
import           Reflex.Dom.SemanticUI.RadioGroup
import Reflex.Dom.Core hiding
  ( checkbox, Checkbox (..), checkbox_value, checkbox_change
  , CheckboxConfig (..), checkboxConfig_attributes, checkboxConfig_setValue
  , HasSetValue (..), HasValue (..), HasAttributes (..)
  , DropdownConfig (..), dropdown_change, dropdown_value
  , link
  )
import Language.Javascript.JSaddle (JSM)
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | The contents of the upstream semantic.min.css stylesheet as a Text value.
semanticCSS :: Text
semanticCSS = $(embedStringFile "lib/semantic.min.css")

--semanticMain :: MonadWidget t m => m () -> JSM ()
semanticMain :: (forall x. Widget x ()) -> JSM ()
semanticMain = mainWidgetWithCss $(embedStringFile "lib/semantic.min.css")

--semanticMain :: MonadWidget t m => m () -> JSM ()
semanticMainWithCss :: ByteString -> (forall x. Widget x ()) -> JSM ()
semanticMainWithCss css = mainWidgetWithCss $ css `mappend` $(embedStringFile "lib/semantic.min.css")

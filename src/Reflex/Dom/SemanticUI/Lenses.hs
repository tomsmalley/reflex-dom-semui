{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE UndecidableInstances #-}

module Reflex.Dom.SemanticUI.Lenses where

import Control.Lens.TH
import Reflex.Dom.SemanticUI.Checkbox
import Reflex.Dom.SemanticUI.Dropdown
import Reflex.Dom.SemanticUI.Header
import Reflex.Dom.SemanticUI.RadioGroup
import Reflex.Dom.SemanticUI.Icon

$(makeFieldsNoPrefix ''Checkbox)
$(makeFieldsNoPrefix ''CheckboxConfig)
$(makeFieldsNoPrefix ''Dropdown)
$(makeFieldsNoPrefix ''DropdownConfig)
$(makeFieldsNoPrefix ''RadioItem)
$(makeFieldsNoPrefix ''RadioItemConfig)
$(makeFieldsNoPrefix ''RadioGroupConfig)

$(makeFieldsNoPrefix ''LabelConfig)

$(makeFieldsNoPrefix ''ImageConfig)
$(makeFieldsNoPrefix ''IconConfig)
$(makeFieldsNoPrefix ''IconsConfig)
$(makeFieldsNoPrefix ''HeaderConfig)

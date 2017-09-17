{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE UndecidableInstances #-}

module Reflex.Dom.SemanticUI.Lenses where

import Control.Lens.TH
import Reflex.Dom.SemanticUI.Checkbox
import Reflex.Dom.SemanticUI.Dropdown
import Reflex.Dom.SemanticUI.RadioGroup

$(makeFieldsNoPrefix ''Checkbox)
$(makeFieldsNoPrefix ''CheckboxConfig)
$(makeFieldsNoPrefix ''Dropdown)
$(makeFieldsNoPrefix ''DropdownConfig)
$(makeFieldsNoPrefix ''RadioItem)
$(makeFieldsNoPrefix ''RadioItemConfig)
$(makeFieldsNoPrefix ''RadioGroupConfig)

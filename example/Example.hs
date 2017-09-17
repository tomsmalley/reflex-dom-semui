{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}

module Example (example) where

import Control.Lens
import Control.Monad (void)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Reflex.Dom.SemanticUI
import Language.Javascript.JSaddle hiding ((!!))

import Debug.Trace

import Example.StateEnum
import Example.CountryEnum

-- | Throughput
data Throughput = Unmetered | Metered Int deriving (Eq, Show)

showThroughput :: Throughput -> Text
showThroughput Unmetered = "Unmetered"
showThroughput (Metered n) = T.pack (show n) <> " mbps max"

-- | Frequency
data Frequency = OnceWeek | TwiceWeek | OnceDay | TwiceDay
  deriving (Eq, Show, Enum, Bounded)

showFreq :: Frequency -> Text
showFreq OnceWeek = "Once a week"
showFreq TwiceWeek = "2-3 times a week"
showFreq OnceDay = "Once a day"
showFreq TwiceDay = "Twice a day"

-- | Contacts
data ContactEnum
  = Jenny | Elliot | Stevie | Christian | Matt | Justen
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

showContact :: ContactEnum -> Text
showContact Jenny = "Jenny Hess"
showContact Elliot = "Elliot Fu"
showContact Stevie = "Stevie Feliciano"
showContact Christian = "Christian"
showContact Matt = "Matt"
showContact Justen = "Justen Kitsune"

renderContact :: MonadWidget t m => ContactEnum -> m ()
renderContact contact = do
  elAttr "img" ("class" =: "ui mini avatar image"
            <> "src" =: ("http://semantic-ui.com/images/avatar/small/"
            <> T.toLower (tshow contact) <> ".jpg")) blank
  text $ showContact contact

-- | Cards
data CardEnum = Visa | Amex | Discover
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

showCard :: CardEnum -> Text
showCard Visa = "Visa"
showCard Amex = "American Express"
showCard Discover = "Discover"

renderCard :: MonadWidget t m => CardEnum -> m ()
renderCard card = do
  elClass "i" (T.toLower (tshow card) <> " icon") blank
  text $ showCard card

checkboxes :: MonadWidget t m => m ()
checkboxes = do

  resetEvent <- divClass "ui top attached segment" $ do
    elClass "h4" "ui header" $ do
      text "Checkboxes"
      uiButton (rightFloated . mini . compact . basic <$> def) $ text "Reset"

  divClass "ui bottom attached segment form" $ do
    divClass "two fields" $ do

      divClass "field" $ do
        el "label" $ text "Checkboxes with labels"
        divClass "ui compact segment" $ do
          cb <- checkbox (text "Normal checkbox") $
            def & setValue .~ (Unchecked <$ resetEvent)
          divClass "ui left pointing label" $ display $ view value cb
        divClass "ui compact segment" $ do
          cb <- checkbox (text "Toggle checkbox (checked by default)") $
            def & types .~ [CbToggle]
                & setValue .~ (Checked <$ resetEvent)
                & initialValue .~ Checked
          divClass "ui left pointing label" $ display $ view value cb
        divClass "ui compact segment" $ do
          cb <- checkbox (text "Slider checkbox") $
            def & types .~ [CbSlider]
                & setValue .~ (Unchecked <$ resetEvent)
          divClass "ui left pointing label" $ display $ view value cb

        el "label" $ text "Fitted checkboxes"
        void $ divClass "ui left floated compact segment" $ do
          checkbox blank $
            def & types .~ [CbFitted]
                & setValue .~ (Unchecked <$ resetEvent)
        void $ divClass "ui left floated compact segment" $ do
          checkbox blank $
            def & types .~ [CbFitted, CbToggle]
                & setValue .~ (Unchecked <$ resetEvent)
        void $ divClass "ui left floated compact segment" $ do
          checkbox blank $
            def & types .~ [CbFitted, CbSlider]
                & setValue .~ (Unchecked <$ resetEvent)

      elAttr "div" ("style" =: "clear: both;") blank


      divClass "field" $ do
        let buttonType = small . compact <$> def
        el "label" $ do
          text "Checkbox states"
        enable <- uiButton (custom "left attached" <$> buttonType) $ text "Enable"
        disable <- uiButton (custom "right attached" <$> buttonType) $ text "Disable"
        let enableEvent = leftmost [Enabled <$ enable, Disabled <$ disable]

        divClass "ui compact segment" $ do
          cb <- checkbox (text "Initially disabled") $
            def & setValue .~ (Unchecked <$ resetEvent)
                & initialEnabled .~ Disabled
                & setEnabled .~ leftmost [enableEvent, Disabled <$ resetEvent]
          divClass "ui left pointing label" $ display $ zipDynWith (,) (view value cb) (view enabled cb)
        divClass "ui compact segment" $ do
          cb <- checkbox (text "Initially disabled (checked by default)") $
            def & types .~ [CbToggle]
                & setValue .~ (Checked <$ resetEvent)
                & initialEnabled .~ Disabled
                & initialValue .~ Checked
                & setEnabled .~ leftmost [enableEvent, Disabled <$ resetEvent]
          divClass "ui left pointing label" $ display $ zipDynWith (,) (view value cb) (view enabled cb)

        indeterminateButton <- uiButton (custom "left attached" <$> buttonType) $ text "Indeterminate"
        determinateButton <- uiButton (custom "right attached" <$> buttonType) $ text "Determinate"
        let indeterminateEvent = leftmost [Indeterminate <$ indeterminateButton, Determinate <$ determinateButton]
        divClass "ui compact segment" $ do
          cb <- checkbox (text "Indeterminate") $
            def & types .~ []
                & setValue .~ (Checked <$ resetEvent)
                & divAttributes .~ "class" =: "master"
                & initialIndeterminate .~ Indeterminate
                & initialValue .~ Checked
                & setIndeterminate .~ leftmost [indeterminateEvent, Indeterminate <$ resetEvent]
          divClass "ui left pointing label" $ display $ zipDynWith (,) (view value cb) (view indeterminate cb)

dropdowns :: MonadWidget t m => m ()
dropdowns = do

  resetEvent <- divClass "ui top attached segment" $ do
    elClass "h4" "ui header" $ do
      text "Dropdowns"
      uiButton (rightFloated . mini . compact . basic <$> def) $ text "Reset"

  divClass "ui bottom attached segment form" $ do
    let makeContact x = (x, DropdownItemConfig (tshow x) $ renderContact x)
        contacts = map makeContact [minBound..maxBound]
        makeCard x = (x, DropdownItemConfig "" $ renderCard x)
        cards = map makeCard [minBound..maxBound]
        makeState x = (x, DropdownItemConfig "" $ text $ showState x)
        states = map makeState [minBound..maxBound]
        makeCountry x = (x, DropdownItemConfig "" $ renderCountry x)
        countries = map makeCountry [minBound..maxBound]

    divClass "two fields" $ do
      divClass "field" $ do
        rec el "label" $ do
              text "Single value"
              divClass "ui left pointing label" $ display card
            card <- uiDropdown cards [DOFSelection] $
              def & placeholder .~ "Card Type"
                  & setValue .~ (Just Visa <$ resetEvent)
                  & initialValue ?~ Visa
        return ()
      divClass "field" $ do
        rec el "label" $ do
              text "Single value, search"
              divClass "ui left pointing label" $ display contact
            contact <- uiDropdown contacts [DOFSearch, DOFSelection] $
              def & placeholder .~ "Saved Contacts"
                  & setValue .~ (Nothing <$ resetEvent)
        return ()

    divClass "field" $ do
      rec el "label" $ do
            text "Multi value"
            divClass "ui left pointing label" $ display card
          card <- uiDropdownMulti cards [DOFSelection] $
            def & placeholder .~ "Card Type"
                & setValue .~ (mempty <$ resetEvent)
      return ()

    divClass "field" $ do
      rec el "label" $ do
            text "Multi value, full-text search"
            divClass "ui left pointing label" $ display contact
          contact <- uiDropdownMulti contacts [DOFSearch, DOFSelection] $
            def & placeholder .~ "Saved Contacts"
                & setValue .~ ([Matt, Elliot] <$ resetEvent)
                & initialValue .~ [Matt, Elliot]
                & fullTextSearch .~ True
      return ()

    divClass "two fields" $ do
      divClass "field" $ do
        rec el "label" $ do
              text "Multi value, limited"
              divClass "ui left pointing label" $ display state
            state <- uiDropdownMulti states [DOFSelection] $
              def & placeholder .~ "States"
                  & setValue .~ (mempty <$ resetEvent)
                  & maxSelections ?~ 3
        return ()
      divClass "field" $ do
        rec el "label" $ do
              text "Multi value, search, hidden labels"
              divClass "ui left pointing label" $ display country
            country <- uiDropdownMulti countries [DOFSearch, DOFSelection] $
              def & placeholder .~ "Country"
                  & setValue .~ (mempty <$ resetEvent)
                  & useLabels .~ False
        return ()

radioGroups :: forall t m. MonadWidget t m => m ()
radioGroups = do

  resetEvent <- divClass "ui top attached segment" $ do
    elClass "h4" "ui header" $ do
      text "Radio Groups"
      uiButton (rightFloated . mini . compact . basic <$> def) $ text "Reset"

  divClass "ui bottom attached segment form" $ do
    let makeFreq x = RadioItem x (text $ showFreq x) def
        freqencies = map makeFreq [minBound..maxBound]
        makeThru x = RadioItem x (text $ showThroughput x) def
        throughputs = map makeThru [Metered 20, Metered 10, Metered 5, Unmetered]

    divClass "field" $ do
      rec el "label" $ do
            text "Normal radio group"
            divClass "ui left pointing label" $ display frequency
          frequency <- divClass "inline fields" $ do
            radioGroup "frequency" freqencies $
              def & setValue .~ (Nothing <$ resetEvent)
      return ()

    divClass "field" $ do
      rec el "label" $ do
            text "Slider group"
            divClass "ui left pointing label" $ display throughput
          throughput <- divClass "grouped fields" $ do
            radioGroup "throughput" throughputs $
              def & initialValue ?~ Unmetered
                  & setValue .~ (Just Unmetered <$ resetEvent)
                  & types .~ [CbSlider]
      return ()

example :: JSM ()
example = mainWidgetWithCss css $ elAttr "div" containerAttrs $ do
  checkboxes
  dropdowns
  radioGroups

  where
    containerAttrs = "class" =: "ui container" <> "style" =: "margin-top: 1em;"
    css = encodeUtf8 semanticCSS

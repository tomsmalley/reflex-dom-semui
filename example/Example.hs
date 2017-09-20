{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Example (example) where

import Control.Lens
import Control.Monad (void, when)
import Control.Monad.Trans (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.FileEmbed
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Reflex.Dom.SemanticUI
import Language.Javascript.JSaddle hiding ((!!))

import Example.StateEnum
import Example.CountryEnum
import Example.QQ

import Debug.Trace

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

  ui $ Header H2 "Checkboxes" $ def & dividing .~ True
  resetEvent <- uiButton (rightFloated . mini . compact . basic <$> def) $ text "Reset"

  divClass "ui two column grid container" $ do

    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn id "Checkboxes with labels" "" [mkExample|
        divClass "ui form" $ do
          normal <- divClass "field" $ checkbox (text "Normal checkbox") $
            def & setValue .~ (Unchecked <$ resetEvent)
          toggle <- divClass "field" $ checkbox (text "Toggle checkbox (checked by default)") $
            def & types .~ [CbToggle]
                & setValue .~ (Checked <$ resetEvent)
                & initialValue .~ Checked
          slider <- divClass "field" $ checkbox (text "Slider checkbox") $
            def & types .~ [CbSlider]
                & setValue .~ (Unchecked <$ resetEvent)
          return $ traverse (view value) [normal, slider, toggle]
        |]

      divClass "column" $ do
        exampleCardDyn id "Fitted checkboxes" "" [mkExample|
        normal <- divClass "ui compact segment" $ checkbox blank $
            def & types .~ [CbFitted]
                & setValue .~ (Unchecked <$ resetEvent)
        slider <- divClass "ui compact segment" $ checkbox blank $
            def & types .~ [CbFitted, CbToggle]
                & setValue .~ (Unchecked <$ resetEvent)
        toggle <- divClass "ui compact segment" $ checkbox blank $
            def & types .~ [CbFitted, CbSlider]
                & setValue .~ (Unchecked <$ resetEvent)
        return $ traverse (view value) [normal, slider, toggle]
        |]

    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn id "Checkbox states" "Checkboxes can be enabled or disabled" [mkExample|
        enable <- uiButton (custom "left attached" <$> def) $ text "Enable"
        disable <- uiButton (custom "right attached" <$> def) $ text "Disable"
        let enableEvent = leftmost [Enabled <$ enable, Disabled <$ disable]
        divClass "ui form" $ do
          normal <- divClass "field" $ checkbox (text "Initially disabled") $
            def & setValue .~ (Unchecked <$ resetEvent)
                & initialEnabled .~ Disabled
                & setEnabled .~ leftmost [enableEvent, Disabled <$ resetEvent]
          toggle <- divClass "field" $ checkbox (text "Initially disabled (checked by default)") $
            def & types .~ [CbToggle]
                & setValue .~ (Checked <$ resetEvent)
                & initialEnabled .~ Disabled
                & initialValue .~ Checked
                & setEnabled .~ leftmost [enableEvent, Disabled <$ resetEvent]
          return $ traverse (\cb -> (\x y -> (x, y)) <$> (view value cb) <*> (view enabled cb)) [normal, toggle]
        |]

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

  ui $ Header H2 "Radio Groups" $ def & dividing .~ True
  resetEvent <- uiButton (rightFloated . mini . compact . basic <$> def) $ text "Reset"

  divClass "ui two column grid container" $ do
    divClass "row" $ do
      divClass "column" $ do
        exampleCardDyn id "Radio group" "" [mkExample|
        let mkRadioItem x = RadioItem x (text $ showFreq x) def
            freqencies = map mkRadioItem [minBound..maxBound]
        divClass "ui form" $ radioGroup "frequency" freqencies $
          def & setValue .~ (Nothing <$ resetEvent)
        |]

      divClass "column" $ do
        exampleCardDyn id "Slider group" "" [mkExample|
        let mkRadioItem x = RadioItem x (text $ T.pack $ show x) def
            throughputs = mkRadioItem <$> [Metered 20, Metered 10, Metered 5, Unmetered]
        divClass "ui form" $ radioGroup "throughput" throughputs $
          def & initialValue ?~ Unmetered
              & setValue .~ (Just Unmetered <$ resetEvent)
              & types .~ [CbSlider]
        |]

  return ()

hscolourCss :: ByteString
hscolourCss = $(embedStringFile "lib/hscolour.css")

exampleCard :: MonadWidget t m => Text -> Text -> (String, m a) -> m a
exampleCard title subtitle (code, widget) = divClass "ui fluid card" $ do
  divClass "content" $ do
    divClass "header" (text title)
    when (subtitle /= "") $ divClass "meta" (text subtitle)
  widgetResult <- divClass "content" widget
  isOpen <- divClass "content" $ toggleUI' $ \isOpen ->
    uiButton def $ if isOpen then text "hide code" else text "show code"
  void $ dyn $ codeEl <$> isOpen
  return widgetResult
  where
    codeEl False = blank
    codeEl True = divClass "content" $ hscode code

exampleCardDyn :: (Show a, MonadWidget t m) => (b -> Dynamic t a) -> Text -> Text
               -> (String, m b) -> m (Dynamic t a)
exampleCardDyn getDyn title subtitle (code, widget) = divClass "ui fluid card" $ do
  divClass "content" $ do
    divClass "header" (text title)
    when (subtitle /= "") $ divClass "meta" (text subtitle)
  widgetResult <- getDyn <$> divClass "content" widget
  divClass "extra content" $ do
    ui $ Header H4 "Value" $ def & header .~ ContentHeader
    dyn $ hscodeInline . show <$> widgetResult
  isOpen <- divClass "content" $ toggleUI' $ \isOpen ->
    uiButton (custom "labeled icon" <$> def) $ if isOpen
      then ui (Icon "minus" def) >> text "Hide Code"
      else ui (Icon "plus" def) >> text "Show Code"
  void $ dyn $ codeEl <$> isOpen
  return widgetResult
  where
    codeEl False = blank
    codeEl True = divClass "content" $ hscode code

example :: JSM ()
example = semanticMainWithCss hscolourCss $ do
  elAttr "div" containerAttrs $ do
    let semanticLogo = Image "https://semantic-ui.com/images/logo.png" $ def
          & size ?~ Massive & rounded ?~ Rounded
    ui $ Header H1 "Semantic UI for Reflex Dom" $ def
      & image ?~ semanticLogo
      & subHeader ?~ "Example app"

    ui $ Header H2 "Icon" def

    $(printDefinition ''Icon)

    ui $ Header H3 "Groups" def

    divClass "ui three column grid container" $ do
      divClass "row" $ do
        divClass "column" $ do
          exampleCard "Icons" "Several icons can be used together as a group" [mkExample|
          ui $ Icons
            [ Icon "circle" $ def & size ?~ Big & color ?~ Blue
            , Icon "car" $ def & inverted .~ True
            ] $ def & size ?~ Huge
          ui $ Icons
            [ Icon "thin circle" $ def & size ?~ Big
            , Icon "user" def
            ] $ def & size ?~ Huge
          ui $ Icons
            [ Icon "certificate" $ def
                & size ?~ Big & loading .~ True
                & color ?~ Grey & inverted .~ True
            , Icon "cloud download" def
            ] $ def & size ?~ Huge
          |]

        divClass "column" $ do
          exampleCard "Corner Icon" "A group of icons can display a smaller corner icon"
            [mkExample|
          ui $ Header H2 "Add on Twitter" $ def
            & icon ?~ Icons
                [ Icon "twitter" def
                , Icon "corner add" $ def & inverted .~ True
                ] (def & size ?~ Large)
          |]

    checkboxes
    dropdowns
    radioGroups

  where
    containerAttrs = "class" =: "ui container" <> "style" =: "margin-top: 1em;"
    css = encodeUtf8 semanticCSS

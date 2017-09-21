{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Example (example) where

import GHC.Tuple
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

section :: MonadWidget t m => Text -> m () -> m ()
section heading child = do
  ui $ Header H2 heading $ def
    & dividing .~ True
--    & id ?~ T.toLower heading
  child

checkboxes :: MonadWidget t m => m ()
checkboxes = section "Checkbox" $ do

  elAttr "a" ("id" =: "checkbox" <> "class" =: "anchor") blank

  divClass "ui two column stackable grid" $ do
    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn id "Checkbox" "Standard checkbox styles" $ [mkExample|
        \resetEvent -> do
          normal <- divClass "ui compact segment"
            $ checkbox (text "Normal checkbox")
            $ def & setValue .~ (Unchecked <$ resetEvent)
          toggle <- divClass "ui compact segment"
            $ checkbox (text "Toggle checkbox (checked by default)")
            $ def & types .~ [CbToggle]
                  & setValue .~ (Checked <$ resetEvent)
                  & initialValue .~ Checked
          slider <- divClass "ui compact segment"
            $ checkbox (text "Slider checkbox")
            $ def & types .~ [CbSlider]
                  & setValue .~ (Unchecked <$ resetEvent)
          return $ traverse (view value) [normal, toggle, slider]
        |]

      divClass "column" $ do
        exampleCardDyn id "Disabled checkbox" "Checkboxes can be enabled or disabled" [mkExample|
        \resetEvent -> do
          enable <- uiButton (custom "left attached" <$> def) $ text "Enable"
          disable <- uiButton (custom "right attached" <$> def) $ text "Disable"
          let enableEvent = leftmost [Enabled <$ enable, Disabled <$ disable]
          normal <- divClass "ui segment"
            $ checkbox (text "Initially disabled")
            $ def & setValue .~ (Unchecked <$ resetEvent)
                  & initialEnabled .~ Disabled
                  & setEnabled .~ leftmost [enableEvent, Disabled <$ resetEvent]
          toggle <- divClass "ui segment"
            $ checkbox (text "Initially disabled (checked by default)")
            $ def & types .~ [CbToggle]
                  & setValue .~ (Checked <$ resetEvent)
                  & initialEnabled .~ Disabled
                  & initialValue .~ Checked
                  & setEnabled .~ leftmost [enableEvent, Disabled <$ resetEvent]
          return $ traverse (\cb -> (,) <$> view value cb <*> view enabled cb) [normal, toggle]
        |]

    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn id "Checkbox states" "Checkboxes can be indeterminate" [mkExample|
        \resetEvent -> do
          indeterminateButton <- uiButton (custom "left attached" <$> def) $ text "Indeterminate"
          determinateButton <- uiButton (custom "right attached" <$> def) $ text "Determinate"
          let indeterminateEvent = leftmost [Indeterminate <$ indeterminateButton, Determinate <$ determinateButton]
          cb <- divClass "ui compact segment"
            $ checkbox (text "Indeterminate")
            $ def & types .~ []
                  & setValue .~ (Checked <$ resetEvent)
                  & initialIndeterminate .~ Indeterminate
                  & initialValue .~ Checked
                  & setIndeterminate .~ leftmost [indeterminateEvent, Indeterminate <$ resetEvent]
          return $ (,) <$> view value cb <*> view indeterminate cb
        |]

      divClass "column" $ do
        exampleCardDyn id "Fitted checkbox" "A fitted checkbox does not leave padding for a label" [mkExample|
        \resetEvent -> do
          normal <- divClass "ui compact segment"
            $ checkbox blank
            $ def & types .~ [CbFitted]
                  & setValue .~ (Unchecked <$ resetEvent)
          slider <- divClass "ui compact segment"
            $ checkbox blank
            $ def & types .~ [CbFitted, CbToggle]
                  & setValue .~ (Unchecked <$ resetEvent)
          toggle <- divClass "ui compact segment"
            $ checkbox blank
            $ def & types .~ [CbFitted, CbSlider]
                  & setValue .~ (Unchecked <$ resetEvent)
          return $ traverse (view value) [normal, slider, toggle]
        |]

    return ()

dropdowns :: MonadWidget t m => m ()
dropdowns = section "Dropdown" $ do

  elAttr "a" ("id" =: "dropdown" <> "class" =: "ui anchor") blank

  let makeContact x = (x, DropdownItemConfig (tshow x) $ renderContact x)
      contacts = map makeContact [minBound..maxBound]
      makeCard x = (x, DropdownItemConfig "" $ renderCard x)
      cards = map makeCard [minBound..maxBound]
      makeState x = (x, DropdownItemConfig "" $ text $ showState x)
      states = map makeState [minBound..maxBound]
      makeCountry x = (x, DropdownItemConfig "" $ renderCountry x)
      countries = map makeCountry [minBound..maxBound]

  divClass "ui two column stackable grid" $ do
    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn id "Single value" "" [mkExample|
        \resetEvent -> uiDropdown cards [DOFSelection]
          $ def & placeholder .~ "Card Type"
                & setValue .~ (Just Visa <$ resetEvent)
                & initialValue ?~ Visa
        |]

      divClass "column" $ do
        exampleCardDyn id "Single value, search" "" [mkExample|
        \resetEvent -> uiDropdown contacts [DOFSearch, DOFSelection]
          $ def & placeholder .~ "Saved Contacts"
                & setValue .~ (Nothing <$ resetEvent)
        |]

    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn id "Multi value" "" [mkExample|
        \resetEvent -> uiDropdownMulti cards [DOFSelection]
          $ def & placeholder .~ "Card Type"
                & setValue .~ (mempty <$ resetEvent)
        |]

      divClass "column" $ do
        exampleCardDyn id "Multi value, full-text search" "" [mkExample|
        \resetEvent -> uiDropdownMulti contacts [DOFSearch, DOFSelection]
          $ def & placeholder .~ "Saved Contacts"
                & setValue .~ ([Matt, Elliot] <$ resetEvent)
                & initialValue .~ [Matt, Elliot]
                & fullTextSearch .~ True
        |]

    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn id "Multi value, limited " "" [mkExample|
        \resetEvent -> uiDropdownMulti states [DOFSelection]
          $ def & placeholder .~ "States"
                & setValue .~ (mempty <$ resetEvent)
                & maxSelections ?~ 3
        |]

      divClass "column" $ do
        exampleCardDyn id "Multi value, search, hidden labels " "" [mkExample|
        \resetEvent -> uiDropdownMulti countries [DOFSearch, DOFSelection]
          $ def & placeholder .~ "Country"
                & setValue .~ (mempty <$ resetEvent)
                & useLabels .~ False
        |]

  return ()

radioGroups :: forall t m. MonadWidget t m => m ()
radioGroups = section "Radio Group" $ do

  elAttr "a" ("id" =: "radio-group" <> "class" =: "ui anchor") blank

  divClass "ui two column stackable grid" $ do
    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn id "Radio group" "" [mkExample|
        \resetEvent -> do
          let mkRadioItem x = RadioItem x (text $ showFreq x) def
              freqencies = map mkRadioItem [minBound..maxBound]
          divClass "ui form" $ radioGroup "frequency" freqencies $
            def & setValue .~ (Nothing <$ resetEvent)
        |]

      divClass "column" $ do
        exampleCardDyn id "Slider group" "" [mkExample|
        \resetEvent -> do
          let mkRadioItem x = RadioItem x (text $ T.pack $ show x) def
              throughputs = mkRadioItem <$> [Metered 20, Metered 10, Metered 5, Unmetered]
          divClass "ui form" $ radioGroup "throughput" throughputs $
            def & initialValue ?~ Unmetered
                & setValue .~ (Just Unmetered <$ resetEvent)
                & types .~ [CbSlider]
        |]

  return ()

icons :: MonadWidget t m => m ()
icons = section "Icon" $ do

  $(printDefinition ''Icon)
  $(printDefinition ''IconConfig)
  $(printDefinition ''IconsConfig)

  ui $ Header H3 "Groups" def

  divClass "ui equal width stackable grid" $ do

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

hscolourCss :: ByteString
hscolourCss = $(embedStringFile "lib/hscolour.css")

exampleCard :: MonadWidget t m => Text -> Text -> (String, m a) -> m a
exampleCard headerText subtitle (code, widget) = divClass "ui fluid card" $ do
  isOpen <- divClass "content" $ do
    let iconConf = def & link .~ True & floated ?~ RightFloated
    isOpen <- (=<<) (toggle False) $ ui $ Icon "code" $ iconConf & title ?~ "Show Code"
    divClass "header" (text headerText)
    when (subtitle /= "") $ divClass "meta" (text subtitle)
    return isOpen
  widgetResult <- divClass "content" widget
  void $ dyn $ codeEl <$> isOpen
  return widgetResult
  where
    codeEl False = blank
    codeEl True = divClass "content" $ hscode code

exampleCardDyn :: (Show a, MonadWidget t m) => (b -> Dynamic t a) -> Text -> Text
               -> (String, Event t () -> m b) -> m (Dynamic t a)
exampleCardDyn getDyn headerText subtitle (code, widget) = divClass "ui fluid card" $ do
  (isOpen, resetEvent) <- divClass "content" $ do
    let iconConf = def & link .~ True & floated ?~ RightFloated
    isOpen <- (=<<) (toggle False) $ ui $ Icon "code" $ iconConf & title ?~ "Show Code"
    resetEvent <- ui $ Icon "refresh" $ iconConf & title ?~ "Reset"
    divClass "header" (text headerText)
    when (subtitle /= "") $ divClass "meta" (text subtitle)
    return (isOpen, resetEvent)
  widgetResult <- getDyn <$> divClass "content" (widget resetEvent)
  divClass "extra content" $ do
    ui $ Header H4 "Value" $ def & header .~ ContentHeader
    dyn $ hscodeInline . show <$> widgetResult
  void $ dyn $ codeEl <$> isOpen
  return widgetResult
  where
    codeEl False = blank
    codeEl True = divClass "content" $ hscode code

example :: JSM ()
example = semanticMainWithCss hscolourCss $ do
  elAttr "div" ("id" =: "masthead" <> "class" =: "ui vertical segment") $ do
    divClass "ui container" $ do
      let semanticLogo = Image "https://semantic-ui.com/images/logo.png" $ def
            & size ?~ Massive & rounded ?~ Rounded
      ui $ Header H1 "Semantic UI for Reflex Dom" $ def
        & image ?~ semanticLogo
        & subHeader ?~ "Documentation and examples"

  elAttr "div" ("id" =: "main" <> "class" =: "ui container") $ do
    divClass "ui dividing right rail" $ do
      divClass "ui sticky" $ do -- TODO make sticky work
        ui $ Header H4 "Menu" def
        divClass "ui vertical following fluid accordion text menu" $ do
          divClass "item" $ text "Checkbox"
          divClass "item" $ text "Dropdown"
          divClass "item" $ text "Radio Group"

    checkboxes
    dropdowns
    radioGroups
    icons

  where
    containerAttrs = "class" =: "ui container" <> "style" =: "margin-top: 1em;"
    css = encodeUtf8 semanticCSS

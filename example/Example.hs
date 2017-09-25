{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures       #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds   #-}

module Example (example) where

import GHC.Tuple
import Control.Lens
import Control.Monad ((<=<), void, when)
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
  ui $ Header H2 (text heading) $ def
    & dividing .~ True
--    & id ?~ T.toLower heading
  child

checkboxes :: forall t m. MonadWidget t m => m ()
checkboxes = section "Checkbox" $ do

  elAttr "a" ("id" =: "checkbox" <> "class" =: "anchor") blank

  divClass "ui two column stackable grid" $ do
    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn id "Checkbox" "Standard checkbox styles" $ [mkExample|
        \resetEvent -> do
          normal <- divClass "ui compact segment"
            $ ui $ Checkbox "Normal checkbox"
            $ def & setValue .~ (Unchecked <$ resetEvent)
          toggle <- divClass "ui compact segment"
            $ ui $ Checkbox "Toggle checkbox (checked by default)"
            $ def & types .~ [Toggle]
                  & setValue .~ (Checked <$ resetEvent)
                  & initialValue .~ Checked
          slider <- divClass "ui compact segment"
            $ ui $ Checkbox "Slider checkbox"
            $ def & types .~ [Slider]
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
            $ ui $ Checkbox "Initially disabled"
            $ def & setValue .~ (Unchecked <$ resetEvent)
                  & initialEnabled .~ Disabled
                  & setEnabled .~ leftmost [enableEvent, Disabled <$ resetEvent]
          toggle <- divClass "ui segment"
            $ ui $ Checkbox "Initially disabled (checked by default)"
            $ def & types .~ [Toggle]
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
            $ ui $ Checkbox "Indeterminate"
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
            $ ui $ Checkbox ""
            $ def & types .~ [Fitted]
                  & setValue .~ (Unchecked <$ resetEvent)
          slider <- divClass "ui compact segment"
            $ ui $ Checkbox ""
            $ def & types .~ [Fitted, Toggle]
                  & setValue .~ (Unchecked <$ resetEvent)
          toggle <- divClass "ui compact segment"
            $ ui $ Checkbox ""
            $ def & types .~ [Fitted, Slider]
                  & setValue .~ (Unchecked <$ resetEvent)
          return $ traverse (view value) [normal, slider, toggle]
        |]

    return ()

dropdowns :: MonadWidget t m => m ()
dropdowns = section "Dropdown" $ do



  elAttr "a" ("id" =: "dropdown" <> "class" =: "ui anchor") blank

  let makeContact x = (x, DropdownItemConfig' (tshow x) $ renderContact x)
      contacts = map makeContact [minBound..maxBound]
      makeCard x = (x, DropdownItemConfig' "" $ renderCard x)
      cards = map makeCard [minBound..maxBound]
      makeState x = (x, DropdownItemConfig' "" $ text $ showState x)
      states = map makeState [minBound..maxBound]
      makeCountry x = (x, DropdownItemConfig' "" $ renderCountry x)
      countries = map makeCountry [minBound..maxBound]

  divClass "ui two column stackable grid" $ do
    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn id "Single value inline menu" "A dropdown can be formatted to appear inline in other content" [mkExample|
        \resetEvent -> el "span" $ do
          let mkItem contact = DropdownItem contact (showContact contact) $ def
                & image ?~ Image (src contact) (def & avatar .~ True)
              src contact = "http://semantic-ui.com/images/avatar/small/"
                          <> T.toLower (tshow contact) <> ".jpg"
              contacts = map mkItem [minBound..maxBound]
          text $ "Show me posts by "
          ui $ DropdownDef contacts
            $ pure Jenny
                & placeholder .~ "Saved Contacts"
                & inline .~ True
                & setValue .~ (Jenny <$ resetEvent)
        |]

      divClass "column" $ do
        exampleCardDyn id "Single value" "" [mkExample|
        \resetEvent -> do
          let mkItem card = DropdownItem card (showCard card) $ def
                & icon ?~ Icon (T.toLower $ tshow card) def
              cards = map mkItem [minBound..maxBound]
          ui $ Dropdown cards
            $ def & placeholder .~ "Card Type"
                  & setValue .~ (Just Visa <$ resetEvent)
                  & initialValue ?~ Visa
                  & selection .~ True
        |]

--  elAttr "img" ("class" =: "ui mini avatar image"

      divClass "column" $ do
        exampleCardDyn id "Single value, search" "" [mkExample|
        \resetEvent -> do
          let mkItem contact = DropdownItem contact (showContact contact) $ def
                & image ?~ Image (src contact) (def & size ?~ Mini & avatar .~ True)
              src contact = "http://semantic-ui.com/images/avatar/small/"
                          <> T.toLower (tshow contact) <> ".jpg"
              contacts = map mkItem [minBound..maxBound]
          ui $ Dropdown contacts
            $ def & placeholder .~ "Saved Contacts"
                  & setValue .~ (Nothing <$ resetEvent)
                  & selection .~ True
                  & search .~ True
                  & textOnly .~ True
        |]

    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn id "Multi value" "" [mkExample|
        \resetEvent -> uiDropdownMulti cards
          $ def & placeholder .~ "Card Type"
                & setValue .~ (mempty <$ resetEvent)
                & selection .~ True
        |]

      divClass "column" $ do
        exampleCardDyn id "Multi value, full-text search" "" [mkExample|
        \resetEvent -> uiDropdownMulti contacts
          $ def & placeholder .~ "Saved Contacts"
                & setValue .~ ([Matt, Elliot] <$ resetEvent)
                & initialValue .~ [Matt, Elliot]
                & fullTextSearch .~ True
                & selection .~ True
                & search .~ True
        |]

    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn id "Multi value, limited " "" [mkExample|
        \resetEvent -> uiDropdownMulti states
          $ def & placeholder .~ "States"
                & setValue .~ (mempty <$ resetEvent)
                & maxSelections ?~ 3
                & selection .~ True
        |]

      divClass "column" $ do
        exampleCardDyn id "Multi value, search, hidden labels " "" [mkExample|
        \resetEvent -> uiDropdownMulti countries
          $ def & placeholder .~ "Country"
                & setValue .~ (mempty <$ resetEvent)
                & useLabels .~ False
                & selection .~ True
                & search .~ True
        |]

  return ()

data Favourite
  = Haskell
  | Semantic
  | Reflex
  deriving (Eq, Show)

menu :: forall t m. MonadWidget t m => m ()
menu = section "Menu" $ do

  exampleCardDyn id "Header" "A menu item may include a header or may itself be a header" [mkExample|
  \resetEvent -> do
    (selected, HNil) <- ui $ Menu
      ( SubMenu (constDyn $ part $ Header H3 (text "Products") $ def & header .~ ContentHeader)
        ( MenuItem "Enterprise" (constDyn $ text "Enterprise") def
        $ MenuItem "Consumer" (constDyn $ text "Consumer") def
        $ MenuBase )
      $ SubMenu (constDyn $ part $ Header H3 (text "CMS Solutions") $ def & header .~ ContentHeader)
        ( MenuItem "Rails" (constDyn $ text "Rails") def
        $ MenuItem "Python" (constDyn $ text "Python") def
        $ MenuItem "PHP" (constDyn $ text "PHP") def
        $ MenuBase )
      $ SubMenu (constDyn $ part $ Header H3 (text "Hosting") $ def & header .~ ContentHeader)
        ( MenuItem "Shared" (constDyn $ text "Shared") def
        $ MenuItem "Dedicated" (constDyn $ text "Dedicated") def
        $ MenuBase )
      $ SubMenu (constDyn $ part $ Header H3 (text "Support") $ def & header .~ ContentHeader)
        ( MenuItem "E-mail Support" (constDyn $ text "E-mail Support") def
        $ MenuItem "FAQs" (constDyn $ text "FAQs") def
        $ MenuBase )
      $ MenuBase )
      $ def & vertical .~ True & setValue .~ (Nothing <$ resetEvent)
    return selected
   |]

  exampleCardDyn id "Sub Menu" "A menu item may contain another menu nested inside that acts as a grouped sub-menu" [mkExample|
  \resetEvent -> do
    (selected, search `HCons` HNil) <- ui $ Menu
      ( MenuWidget ( do
          ti <- uiTextInput def (def { _textInputConfig_attributes = constDyn $ "placeholder" =: "Search..." })
          return $ _textInput_value ti
        ) def
      $ SubMenu (constDyn $ text "Home")
        ( MenuItem "Search" (constDyn $ text "Search") def
        $ MenuItem "Add" (constDyn $ text "Add") def
        $ MenuItem "Remove" (constDyn $ text "Remove") def
        $ MenuDropdown "More"
          ( MenuItem "Edit" (constDyn $ text "Edit") def
          $ MenuItem "Tag" (constDyn $ text "Tag") def
          $ MenuBase )
        $ MenuBase )
      $ MenuItem "Browse" (constDyn $ do
          ui $ Icon "grid layout" def
          text "Browse"
        ) def
      $ MenuItem "Messages" (constDyn $ text "Messages") def
      $ MenuDropdown "More"
        ( MenuItem "Edit Profile" (constDyn $ ui (Icon "edit" def) >> text "Edit Profile") def
        $ MenuItem "Choose Language" (constDyn $ ui (Icon "globe" def) >> text "Choose Language") def
        $ MenuItem "Account Settings" (constDyn $ ui (Icon "settings" def) >> text "Account Settings") def
        $ MenuDropdown "Even More"
          ( MenuItem "Contact Us" (constDyn $ ui (Icon "talk" def) >> text "Contact Us") def
          $ MenuItem "Make a Suggestion" (constDyn $ ui (Icon "idea" def) >> text "Make a Suggestion") def
          $ MenuBase )
        $ MenuBase )
      $ MenuBase )
      $ def & vertical .~ True
    return $ (,) <$> selected <*> search
   |]

  exampleCardDyn id "Arbitrary Widgets" "An item may contain an arbitrary widget with optional capture of the result" [mkExample|
  \resetEvent -> do
    let makeHeader doIcon txt = elAttr "div" ("style" =: "margin-bottom: 0.7em") $ do
          when doIcon $ void $ ui $ Icon "external" $ def & floated ?~ RightFloated
          elAttr "span" ("style" =: "font-weight: bold; text-size: 1.1em") $ text txt
        favs = map (\x -> (x, DropdownItemConfig' x $ text x)) ["Haskell", "Semantic UI", "Reflex"]
    -- Type signature required or the value is ambiguous
    (selected :: Dynamic t (Maybe Int), fav `HCons` HNil) <- ui $ Menu
      ( MenuWidget_ ( do
          makeHeader True "Haskell"
          el "p" $ text "Purely functional programming language"
        ) (def & link .~ Link "http://haskell.org/")
      $ MenuWidget_ ( do
          makeHeader True "Semantic UI"
          el "p" $ text "UI development framework designed for theming"
        ) (def & link .~ Link "http://semantic-ui.com/")
      $ MenuWidget_ ( do
          makeHeader True "Reflex"
          el "p" $ text "Higher-order functional reactive programming"
        ) (def & link .~ Link "http://hackage.haskell.org/package/reflex")
      $ MenuWidget ( do
          makeHeader False "Favourite"
          uiDropdown favs $ def
            & placeholder .~ "Pick your favourite..."
            & selection .~ True
            & fluid .~ True
        ) (def & link .~ NoLink)
      $ MenuBase )
      $ def & vertical .~ True
    return $ (,) <$> selected <*> fav
   |]

  exampleCardDyn id "Secondary Menu" "A menu can adjust its appearance to de-emphasize its contents" [mkExample|
  \resetEvent -> do
    (selected, search `HCons` _) <- ui $ MenuDef
      ( MenuItem "Home" (constDyn $ text "Home") def
      $ MenuItem "Messages" (constDyn $ text "Messages") def
      $ MenuItem "Friends" (constDyn $ text "Friends") def
      $ (MenuSub (def & customMenu ?~ "right")
          ( MenuWidget (uiTextInput def def) (def & link .~ NoLink)
          $ MenuItem "Logout" (constDyn $ text "Logout") def
          $ MenuBase )
        )
      $ MenuBase
      ) $ initMenuConfig "Home"
        & customMenu ?~ "secondary" & setValue .~ ("Home" <$ resetEvent)
    return $ (,) <$> selected <*> _textInput_value search
  |]

  exampleCardDyn id "Secondary Menu" "A menu can adjust its appearance to de-emphasize its contents" [mkExample|
  \resetEvent -> do
    (selected, search `HCons` _) <- ui $ Menu
      ( MenuItem "Home" (constDyn $ text "Home") def
      $ MenuItem "Messages" (constDyn $ text "Messages") def
      $ MenuItem "Friends" (constDyn $ text "Friends") def
      $ (MenuSub (def & customMenu ?~ "right")
          $ MenuWidget (divClass "item" $ uiTextInput def def) def
          . MenuItem "Logout" (constDyn $ text "Logout") def
          $ MenuBase)
      $ MenuBase
      ) $ def & customMenu ?~ "secondary" & setValue .~ (Nothing <$ resetEvent)
    return $ (,) <$> selected <*> _textInput_value search
  |]

  $(printDefinition stripParens ''Menu)
  -- $(printDefinition id ''MenuItems)
  $(printDefinition stripParens ''MenuConfig)
  $(printDefinition stripParens ''MenuItemConfig)

  exampleCardDyn id "Vertical Menu" "A vertical menu displays elements vertically" [mkExample|
  \resetEvent -> do
    inboxCount <- count <=< uiButton def $ text "Add inbox item"
    spamCount <- count <=< uiButton def $ text "Add spam item"
    updatesCount <- count <=< uiButton def $ text "Add updates item"
    let renderItem label classes count = do
          text label
          divClass (T.unwords $ "ui" : "label" : classes) $ text $ T.pack $ show count
    fmap fst $ ui $ Menu
      ( MenuItem "Inbox"
          (renderItem "Inbox" ["teal left pointing"] <$> inboxCount)
          (def & color ?~ Teal)
      $ MenuItem "Spam" (renderItem "Spam" [] <$> spamCount) def
      $ MenuItem "Updates" (renderItem "Updates" [] <$> updatesCount) def
      $ MenuWidget (uiTextInput (custom "transparent icon" <$> def) def { _textInputConfig_attributes = constDyn $ "placeholder" =: "Search mail..." }) def
      $ MenuBase
      ) $ def & setValue .~ (Just "Inbox" <$ resetEvent)
              & initialValue ?~ "Inbox"
              & vertical .~ True
  |]

  return ()

radioGroups :: forall t m. MonadWidget t m => m ()
radioGroups = section "Radio Group" $ do

  $(printDefinition id ''RadioGroup)
--  $(printDefinition id ''RadioGroupConfig)
--  $(printDefinition id ''RadioItem)
--  $(printDefinition id ''RadioItemConfig)

  elAttr "a" ("id" =: "radio-group" <> "class" =: "ui anchor") blank

  divClass "ui two column stackable grid" $ do
    divClass "row" $ do

      divClass "column" $ do
        exampleCardDyn id "Radio group" "" [mkExample|
        \resetEvent -> do
          let mkRadioItem x = RadioItem x (showFreq x) def
              freqencies = map mkRadioItem [minBound..maxBound]
          divClass "ui form" $ ui $ RadioGroup "frequency" freqencies $
            def & setValue .~ (Nothing <$ resetEvent)
        |]

      divClass "column" $ do
        exampleCardDyn id "Slider group" "" [mkExample|
        \resetEvent -> do
          let mkRadioItem x = RadioItem x (T.pack $ show x) def
              throughputs = mkRadioItem <$> [Metered 20, Metered 10, Metered 5, Unmetered]
          divClass "ui form" $ ui $ RadioGroup "throughput" throughputs $
            def & initialValue ?~ Unmetered
                & setValue .~ (Just Unmetered <$ resetEvent)
                & types .~ [Slider]
        |]

  return ()

icons :: MonadWidget t m => m ()
icons = section "Icon" $ do

  $(printDefinition id ''Icon)
  $(printDefinition id ''IconConfig)
  $(printDefinition id ''IconsConfig)

  ui $ Header H3 (text "Groups") def

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
        ui $ Header H2 (text "Add on Twitter") $ def
          & icon ?~ Icons
              [ Icon "twitter" def
              , Icon "corner add" $ def & inverted .~ True
              ] (def & size ?~ Large)
        |]

hscolourCss :: ByteString
hscolourCss = $(embedStringFile "lib/hscolour.css")

exampleCard :: forall t m a. MonadWidget t m => Text -> Text -> (String, m a) -> m a
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

exampleCardDyn :: forall t m a b. (Show a, MonadWidget t m)
               => (b -> Dynamic t a) -> Text -> Text
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
    ui $ Header H4 (text "Value") $ def & header .~ ContentHeader
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
      ui $ Header H1 (text "Semantic UI for Reflex Dom") $ def
        & image ?~ semanticLogo
        & subHeader ?~ text "Documentation and examples"

  elAttr "div" ("id" =: "main" <> "class" =: "ui container") $ do
    divClass "ui dividing right rail" $ do
      divClass "ui sticky" $ do -- TODO make sticky work
        ui $ Header H4 (text "Menu") def
        divClass "ui vertical following fluid accordion text menu" $ do
          divClass "item" $ text "Checkbox"
          divClass "item" $ text "Dropdown"
          divClass "item" $ text "Radio Group"

    menu
    checkboxes
    dropdowns
    radioGroups
    icons

  where
    containerAttrs = "class" =: "ui container" <> "style" =: "margin-top: 1em;"
    css = encodeUtf8 semanticCSS

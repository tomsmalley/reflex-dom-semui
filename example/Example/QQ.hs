{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Example.QQ where

import Language.Haskell.TH (ExpQ, stringE)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.Meta
import Language.Haskell.Meta.Utils

import Language.Haskell.HsColour.Classify (tokenise)
import Language.Haskell.HsColour.CSS (renderToken)

import Control.Monad (void)
import Control.Lens ((?~), (.~), (&))

import Reflex.Dom.SemanticUI
import Reflex.Dom.SemanticUI.Common
import qualified Data.Text as T

ex :: QuasiQuoter
ex = QuasiQuoter
  { quoteExp = \ex -> [|exampleWrapper ex $(eitherQ id (parseExp $ "do\n" ++ ex))|]
  , quotePat = const $ error "ex: not an expression"
  , quoteType = const $ error "ex: not an expression"
  , quoteDec = const $ error "ex: not an expression"
  }

exampleWrapper :: MonadWidget t m => String -> m a -> m a
exampleWrapper code widget = divClass "ui container" $ do
  rec
    (isOpen, widgetResult) <- divClass "ui top attached segment" $ do
      isOpen <- toggleUI $ \isOpen ->
        Label (if isOpen then "hide code" else "show code") $ def
          & hAttached ?~ RightAttached & vAttached ?~ TopAttached & link .~ True
      widgetResult <- elAttr "div" ("style" =: "margin-top: 0 !important;") widget
      return (isOpen, widgetResult)
  void $ dyn $ codeEl <$> isOpen
  return widgetResult
  where
    codeEl False = blank
    codeEl True = void
      . divClass "ui bottom attached segment"
      . elAttr "code" ("class" =: "haskell")
      . elDynHtml' "pre" . constDyn . T.strip . T.pack
      . concatMap renderToken . tokenise $ unindent code

-- | Strips indentation spaces from a string
unindent :: String -> String
unindent str = unlines $ map (drop $ minSpaces loc) loc
  where loc = lines str

-- | Counts the smallest number of spaces that any string in the list is
-- indented by
minSpaces :: [String] -> Int
minSpaces = minimum . map (length . takeWhile (== ' ')) . filter (/= "")

-- | Toggle button in the form of a plus/minus icon. Is 'True' when displaying a
-- minus, 'False' when displaying a plus.
toggleUI :: (UI t m a, MonadWidget t m, Return t m a ~ Event t b) => (Bool -> a) -> m (Dynamic t Bool)
toggleUI element = do
  rec
    updateEvent <- dyn $ ui . element <$> showing
    clickEvent <- switchPromptly never updateEvent
    showing <- toggle False clickEvent
  return showing

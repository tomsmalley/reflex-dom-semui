{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Example.QQ where

import Language.Haskell.TH (ExpQ, stringE, reify, pprint)
import Language.Haskell.TH.Syntax (Name)
import Language.Haskell.TH.Quote
import Language.Haskell.Exts hiding (parseExp, Name)
-- For parsing the haskell strings to template-haskell AST
import Language.Haskell.Meta
import Language.Haskell.Meta.Utils

import Data.Char (isUpper, isAlpha)

import Language.Haskell.HsColour.Classify (tokenise)
import Language.Haskell.HsColour.CSS (renderToken)

import Control.Monad (void, when)
import Control.Lens ((?~), (.~), (&))

import Reflex.Dom.SemanticUI hiding (parseType)
import Reflex.Dom.SemanticUI.Common
import Data.Text (Text)
import qualified Data.Text as T

import Debug.Trace

printDefinition :: Name -> ExpQ
printDefinition name = do
  info <- reify name
  let mode = defaultParseMode
        { baseLanguage = Haskell2010
        , extensions = EnableExtension <$> [TypeFamilies, ExistentialQuantification, DataKinds, GADTs]
        }
      pp = prettyPrint . fromParseResult . parseDeclWithMode mode . stripModules $ pprint info
  [|hscode pp|]

hscode :: MonadWidget t m => String -> m ()
hscode = void . elAttr "code" ("class" =: "haskell")
       . elDynHtml' "pre" . constDyn . hscolour

hscodeInline :: MonadWidget t m => String -> m ()
hscodeInline = void . elAttr "code" ("class" =: "inline haskell")
             . elDynHtml' "pre" . constDyn . hscolour


hscolour :: String -> Text
hscolour = T.strip . T.pack . concatMap renderToken . tokenise . unindent

stripModules :: String -> String
stripModules "" = ""
stripModules (c:s)
  | isUpper c = case span isAlpha (c:s) of
    (_, '.':rest) -> stripModules rest
    (taken, rest) -> taken ++ stripModules rest
  | otherwise = c : stripModules s

ex :: QuasiQuoter
ex = QuasiQuoter
  { quoteExp = \ex -> [|exampleWrapper ex $(eitherQ id (parseExp $ "do\n" ++ ex))|]
  , quotePat = const $ error "ex: not an expression"
  , quoteType = const $ error "ex: not an expression"
  , quoteDec = const $ error "ex: not an expression"
  }

mkExample :: QuasiQuoter
mkExample = QuasiQuoter
  { quoteExp = \ex -> [|(ex, $(eitherQ id $ parseExp $ "do\n" ++ ex))|]
  , quotePat = const $ error "ex: not an expression"
  , quoteType = const $ error "ex: not an expression"
  , quoteDec = const $ error "ex: not an expression"
  }

mkResetExample :: QuasiQuoter
mkResetExample = mkExample
  { quoteExp = \ex -> [|(ex, $(eitherQ id $ parseExp $ "\resetEvent -> do\n" ++ ex))|]
  }

exWithValue :: QuasiQuoter
exWithValue = ex { quoteExp = \ex -> [|exampleWrapperDyn ex $(eitherQ id (parseExp $ "do\n" ++ ex))|] }

exampleWrapperDyn :: (Show a, MonadWidget t m) => String -> m (Dynamic t a) -> m (Dynamic t a)
exampleWrapperDyn code widget = divClass "ui card" $ do
  rec
    (isOpen, widgetResult) <- divClass "content" $ do
      isOpen <- toggleUI $ \isOpen ->
        Label (if isOpen then "hide code" else "show code") $ def
          & hAttached ?~ RightAttached & vAttached ?~ TopAttached & link .~ True
      widgetResult <- elAttr "div" ("style" =: "margin-top: 0 !important;") widget
      return (isOpen, widgetResult)
  void $ dyn $ codeEl <$> isOpen
  divClass "extra content" $ do
    ui $ Header H5 (text "Value") def
    dyn $ hscodeInline . show <$> widgetResult
  return widgetResult
  where
    codeEl False = blank
    codeEl True = divClass "content" $ hscode code

exampleWrapper :: MonadWidget t m => String -> m a -> m a
exampleWrapper code widget = divClass "ui card" $ do
  rec
    (isOpen, widgetResult) <- divClass "content" $ do
      isOpen <- toggleUI $ \isOpen ->
        Label (if isOpen then "hide code" else "show code") $ def
          & hAttached ?~ RightAttached & vAttached ?~ TopAttached & link .~ True
      widgetResult <- elAttr "div" ("style" =: "margin-top: 0 !important;") widget
      return (isOpen, widgetResult)
  void $ dyn $ codeEl <$> isOpen
  return widgetResult
  where
    codeEl False = blank
    codeEl True = divClass "content" $ hscode code

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
toggleUI' :: (MonadWidget t m) => (Bool -> m (Event t b)) -> m (Dynamic t Bool)
toggleUI' element = do
  rec
    updateEvent <- dyn $ element <$> showing
    clickEvent <- switchPromptly never updateEvent
    showing <- toggle False clickEvent
  return showing

-- | Toggle button in the form of a plus/minus icon. Is 'True' when displaying a
-- minus, 'False' when displaying a plus.
toggleUI :: (UI t m a, MonadWidget t m, Return t m a ~ Event t b) => (Bool -> a) -> m (Dynamic t Bool)
toggleUI element = do
  rec
    updateEvent <- dyn $ ui . element <$> showing
    clickEvent <- switchPromptly never updateEvent
    showing <- toggle False clickEvent
  return showing

{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE RecursiveDo              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeFamilyDependencies   #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving     #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Reflex.Dom.SemanticUI.Common where

------------------------------------------------------------------------------
import           Control.Lens ((^.), set, ASetter)
import           Control.Monad (void)
import Data.Default
import Data.String
import Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text as T
import           Language.Javascript.JSaddle
import           Reflex.Dom.Core
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- JSaddle helpers

-- | Javascript console.log
consoleLog :: ToJSVal a => a -> JSM ()
consoleLog a = do
  console <- jsg ("console" :: Text)
  void $ console ^. js1 ("log" :: Text) a

consoleTime :: JSString -> JSM ()
consoleTime a = do
  console <- jsg ("console" :: Text)
  void $ console ^. js1 ("time" :: Text) a

consoleTimeEnd :: JSString -> JSM ()
consoleTimeEnd a = do
  console <- jsg ("console" :: Text)
  void $ console ^. js1 ("timeEnd" :: Text) a

-- | Catch any JSExceptions and log them to the console. Useful for debugging
-- ghc implementations, especially wth jsaddle-warp.
catchJS :: JSM () -> JSM ()
catchJS action = catch action handle
  where handle (JSException e) = consoleLog e

-- | The jQuery function, often used by the alias $(..) in javascript.
jQuery :: ToJSVal a => a -> JSM JSVal
jQuery = jsg1 ("jQuery" :: Text)

------------------------------------------------------------------------------
-- | Temporary...will be moved out of here eventually.
tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Map with index
imap :: (Int -> a -> b) -> [a] -> [b]
imap f = go 0
  where
    go _ [] = []
    go i (x:xs) = f i x : go (succ i) xs

-- | Safe indexing into lists
(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x:_) !? 0 = Just x
(_:xs) !? n = xs !? (n - 1)

------------------------------------------------------------------------------

newtype ClassText = ClassText (Maybe Text) deriving (Eq, Show)

getClass :: ClassText -> Text
getClass (ClassText (Just a)) = a
getClass (ClassText Nothing) = ""

instance IsString ClassText where
  fromString str = ClassText $ Just $ fromString str

instance Monoid ClassText where
  mappend = (<>)
  mempty = ClassText Nothing

instance Semigroup ClassText where
  ClassText (Just a) <> ClassText (Just b) = ClassText $ Just $ a <> " " <> b
  ClassText ma <> ClassText mb = ClassText $ ma <> mb

memptyUnless :: Monoid m => m -> Bool -> m
memptyUnless _ False = mempty
memptyUnless m True = m

class ToClassText a where
  toClassText :: a -> ClassText

instance ToClassText a => ToClassText (Maybe a) where
  toClassText Nothing = mempty
  toClassText (Just a) = toClassText a

nothingIf :: Eq a => a -> Maybe a -> Maybe a
nothingIf x (Just y) | x == y = Nothing
nothingIf _ m = m

justWhen :: Bool -> a -> Maybe a
justWhen True = Just
justWhen False = const Nothing

data Active t a
  = Dynamic !(Dynamic t a)
  | Static !a

instance Reflex t => Functor (Active t) where
  fmap f (Static a) = Static (f a)
  fmap f (Dynamic a) = Dynamic (fmap f a)

instance Reflex t => Applicative (Active t) where
  pure a = Static a
  Static f <*> Static a = Static (f a)
  Dynamic f <*> Dynamic a = Dynamic (f <*> a)
  Static f <*> Dynamic a = Dynamic (f <$> a)
  Dynamic f <*> Static a = Dynamic (($ a) <$> f)

instance IsString a => IsString (Active t a) where
  fromString = Static . fromString

instance Reflex t => IsString (Dynamic t Text) where
  fromString = pure . fromString

runActive :: (MonadWidget t m, UI t m a) => Active t a -> m ()
runActive (Dynamic a) = void $ dyn $ ui_ <$> a
runActive (Static a) = ui_ a

class UI t m a where
  type Return t m a
  ui' :: MonadWidget t m => a -> m (El t, Return t m a)
--  uiDyn :: MonadWidget t m => Dynamic t a -> m (El t, Return t m a)

ui :: (MonadWidget t m, UI t m a) => a -> m (Return t m a)
ui = fmap snd . ui'

ui_ :: (MonadWidget t m, UI t m a) => a -> m ()
ui_ = void . ui

part_ :: (MonadWidget t m, Part t m a) => a -> m ()
part_ = void . part

part :: (MonadWidget t m, Part t m a) => a -> m (Return t m a)
part = fmap snd . part'

class ToItem a where
  toItem :: a -> a

class (ToPart a, UI t m a) => Part t m a where
  part' :: MonadWidget t m => a -> m (El t, Return t m a)
  part' = ui' . toPart

instance (ToPart a, UI t m a) => Part t m a where

instance UI t m Text where
  type Return t m Text = ()
  ui' = el' "" . text

class ToPart a where
  toPart :: a -> a

---------

data RenderWhen t a = NeverRender | RenderWhen { _when :: Dynamic t Bool, _render :: a }

instance Reflex t => Default (RenderWhen t a) where
  def = NeverRender

alwaysRender :: Reflex t => a -> RenderWhen t a
alwaysRender a = RenderWhen
  { _when = pure True
  , _render = a
  }

runRenderWhen
  :: forall t m a. (UI t m a, UI t m a, MonadWidget t m)
  => (a -> m (El t, Return t m a))
  -> RenderWhen t a
  -> m (Dynamic t (Maybe (El t, Return t m a)))
runRenderWhen _ NeverRender = return $ pure Nothing
runRenderWhen render RenderWhen {..} = do
  trimmed :: Dynamic t Bool <- holdUniqDyn _when

  res <- dyn $ fmap (sequence . f) trimmed
  holdDyn Nothing res

  where
    f :: Bool -> Maybe (m (El t, Return t m a))
    f False = Nothing
    f True = Just $ render _render


renderRenderWhen
  :: forall t m a. (UI t m a, MonadWidget t m)
  => RenderWhen t a
  -> m (Dynamic t (Maybe (El t, Return t m a)))
renderRenderWhen NeverRender = return $ pure Nothing
renderRenderWhen RenderWhen {..} = do
  trimmed :: Dynamic t Bool <- holdUniqDyn _when

  res <- dyn $ fmap (sequence . f) trimmed
  holdDyn Nothing res

  where
    f :: Bool -> Maybe (m (El t, Return t m a))
    f False = Nothing
    f True = Just $ ui' _render

---------

(|~) :: Reflex t' => ASetter s t a (Dynamic t' b) -> b -> s -> t
l |~ b = set l (pure b)

(|?~) :: Reflex t' => ASetter s t a (Dynamic t' (Maybe b)) -> b -> s -> t
l |?~ b = set l (pure $ Just b)

data Floated = LeftFloated | RightFloated deriving (Eq, Show)

instance ToClassText Floated where
  toClassText LeftFloated = "left floated"
  toClassText RightFloated = "right floated"

instance UiClassText Floated where
  uiText LeftFloated = "left floated"
  uiText RightFloated = "right floated"

data Size = Mini | Tiny | Small | Medium | Large | Big | Huge | Massive deriving (Eq, Show)

instance ToClassText Size where
  toClassText Mini = "mini"
  toClassText Tiny = "tiny"
  toClassText Small = "small"
  toClassText Medium = "medium"
  toClassText Large = "large"
  toClassText Big = "big"
  toClassText Huge = "huge"
  toClassText Massive = "massive"

instance UiClassText Size where
  uiText = T.toLower . T.pack . show

data HorizontalAttached = LeftAttached | RightAttached deriving (Eq, Show)
data VerticalAttached = TopAttached | BottomAttached deriving (Eq, Show)

instance UiClassText VerticalAttached where
  uiText TopAttached = "top"
  uiText BottomAttached = "bottom"

instance UiClassText HorizontalAttached where
  uiText LeftAttached = "left"
  uiText RightAttached = "right"

instance ToClassText VerticalAttached where
  toClassText TopAttached = "top"
  toClassText BottomAttached = "bottom"

instance ToClassText HorizontalAttached where
  toClassText LeftAttached = "left"
  toClassText RightAttached = "right"

combineAttached :: Maybe VerticalAttached -> Maybe HorizontalAttached -> ClassText
combineAttached Nothing Nothing = mempty
combineAttached mv mh = mconcat [ toClassText mv, toClassText mh, "attached" ]

data Color
  = Red
  | Orange
  | Yellow
  | Olive
  | Green
  | Teal
  | Blue
  | Violet
  | Purple
  | Pink
  | Brown
  | Grey
  | Black
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance ToClassText Color where
  toClassText Red = "red"
  toClassText Orange = "orange"
  toClassText Yellow = "yellow"
  toClassText Olive = "olive"
  toClassText Green = "green"
  toClassText Teal = "teal"
  toClassText Blue = "blue"
  toClassText Violet = "violet"
  toClassText Purple = "purple"
  toClassText Pink = "pink"
  toClassText Brown = "brown"
  toClassText Grey = "grey"
  toClassText Black = "black"

instance UiClassText Color where
  uiText Red = "red"
  uiText Orange = "orange"
  uiText Yellow = "yellow"
  uiText Olive = "olive"
  uiText Green = "green"
  uiText Teal = "teal"
  uiText Blue = "blue"
  uiText Violet = "violet"
  uiText Purple = "purple"
  uiText Pink = "pink"
  uiText Brown = "brown"
  uiText Grey = "grey"
  uiText Black = "black"


-- | A type class for converting data types into appropriate  Semantic UI
-- class text.
class UiClassText a where
  uiText :: a -> Text

------------------------------------------------------------------------------
-- | Passthrough instance for Either
instance (UiClassText a, UiClassText b) => UiClassText (Either a b) where
  uiText (Left a) = uiText a
  uiText (Right b) = uiText b

class UiHasCustom a where
  -- | IMPORTANT: Implementations of this function should use the accompanying
  -- 'addCustom' function to make sure that new values are added on and don't
  -- overwrite anything that was already there.
  custom :: Text -> a -> a

------------------------------------------------------------------------------
-- | Helper function for adding a class item to a custom class field.
addCustom :: Text -> Maybe Text -> Maybe Text
addCustom cls Nothing = Just cls
addCustom cls (Just c) = Just (T.unwords [cls, c])

------------------------------------------------------------------------------
data UiColor
  = UiRed
  | UiOrange
  | UiYellow
  | UiOlive
  | UiGreen
  | UiTeal
  | UiBlue
  | UiViolet
  | UiPurple
  | UiPink
  | UiBrown
  | UiGrey
  | UiBlack
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiColor where
  uiText UiRed = "red"
  uiText UiOrange = "orange"
  uiText UiYellow = "yellow"
  uiText UiOlive = "olive"
  uiText UiGreen = "green"
  uiText UiTeal = "teal"
  uiText UiBlue = "blue"
  uiText UiViolet = "violet"
  uiText UiPurple = "purple"
  uiText UiPink = "pink"
  uiText UiBrown = "brown"
  uiText UiGrey = "grey"
  uiText UiBlack = "black"

class UiHasColor a where
  uiSetColor :: UiColor -> a -> a

instance (Reflex t, UiHasColor a) => UiHasColor (Dynamic t a) where
  uiSetColor c = fmap (uiSetColor c)

red, orange, yellow, olive, green, teal, blue, violet, purple, pink, brown, grey, black
  :: UiHasColor a => a -> a
red = uiSetColor UiRed
orange = uiSetColor UiOrange
yellow = uiSetColor UiYellow
olive = uiSetColor UiOlive
green = uiSetColor UiGreen
teal = uiSetColor UiTeal
blue = uiSetColor UiBlue
violet = uiSetColor UiViolet
purple = uiSetColor UiPurple
pink = uiSetColor UiPink
brown = uiSetColor UiBrown
grey = uiSetColor UiGrey
black = uiSetColor UiBlack

------------------------------------------------------------------------------
data UiEmphasis
  = UiPrimary
  | UiSecondary
  | UiPositive
  | UiNegative
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiEmphasis where
  uiText UiPrimary = "primary"
  uiText UiSecondary = "secondary"
  uiText UiPositive = "positive"
  uiText UiNegative = "negative"

class UiHasEmphasis a where
  uiSetEmphasis :: UiEmphasis -> a -> a

primary, secondary, positive, negative :: UiHasEmphasis a => a -> a
primary = uiSetEmphasis UiPrimary
secondary = uiSetEmphasis UiSecondary
positive = uiSetEmphasis UiPositive
negative = uiSetEmphasis UiNegative

------------------------------------------------------------------------------
data UiInverted = UiInverted
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiInverted where
  uiText UiInverted = "inverted"

class UiHasInverted a where
  inverted :: a -> a

instance (Reflex t, UiHasInverted a) => UiHasInverted (Dynamic t a) where
  inverted = fmap inverted

------------------------------------------------------------------------------
data UiActive = UiActive
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiActive where
  uiText UiActive = "active"

class UiHasActive a where
  active :: a -> a

------------------------------------------------------------------------------
data UiDisabled = UiDisabled
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiDisabled where
  uiText UiDisabled = "disabled"

class UiHasDisabled a where
  disabled :: a -> a

------------------------------------------------------------------------------
data UiSize
  = UiMini
  | UiTiny
  | UiSmall
  | UiMedium
  | UiLarge
  | UiBig
  | UiHuge
  | UiMassive
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiSize where
  uiText UiMini = "mini"
  uiText UiTiny = "tiny"
  uiText UiSmall = "small"
  uiText UiMedium = "medium"
  uiText UiLarge = "large"
  uiText UiBig = "big"
  uiText UiHuge = "huge"
  uiText UiMassive = "massive"

class UiHasSize a where
  uiSetSize :: UiSize -> a -> a

instance (Reflex t, UiHasSize a) => UiHasSize (Dynamic t a) where
  uiSetSize c = fmap (uiSetSize c)

mini, tiny, small, medium, large, big, huge, massive :: UiHasSize a => a -> a
mini = uiSetSize UiMini
tiny = uiSetSize UiTiny
small = uiSetSize UiSmall
medium = uiSetSize UiMedium
large = uiSetSize UiLarge
big = uiSetSize UiBig
huge = uiSetSize UiHuge
massive = uiSetSize UiMassive


------------------------------------------------------------------------------
data UiFlipped
  = UiFlipHoriz
  | UiFlipVert
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiFlipped where
  uiText UiFlipHoriz = "horizontally flipped"
  uiText UiFlipVert = "vertically flipped"

class UiHasFlipped a where
  uiSetFlipped :: UiFlipped -> a -> a

flipHoriz, flipVert :: UiHasFlipped a => a -> a
flipHoriz = uiSetFlipped UiFlipHoriz
flipVert = uiSetFlipped UiFlipVert


------------------------------------------------------------------------------
data UiRotated
  = UiRotateCounterclockwise
  | UiRotateClockwise
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiRotated where
  uiText UiRotateCounterclockwise = "counterclockwise rotated"
  uiText UiRotateClockwise = "clockwise rotated"

class UiHasRotated a where
  uiSetRotated :: UiRotated -> a -> a

counterclockwise, clockwise :: UiHasRotated a => a -> a
counterclockwise = uiSetRotated UiRotateCounterclockwise
clockwise = uiSetRotated UiRotateClockwise


------------------------------------------------------------------------------
data UiAlignment
  = UiLeftAligned
  | UiCenterAligned
  | UiRightAligned
  | UiJustified
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiAlignment where
  uiText UiLeftAligned = "left aligned"
  uiText UiCenterAligned = "center aligned"
  uiText UiRightAligned = "right aligned"
  uiText UiJustified = "justified"

class UiHasAlignment a where
  uiSetAlignment :: UiAlignment -> a -> a

leftAligned, centerAligned, rightAligned, justified :: UiHasAlignment a => a -> a
leftAligned = uiSetAlignment UiLeftAligned
centerAligned = uiSetAlignment UiCenterAligned
rightAligned = uiSetAlignment UiRightAligned
justified = uiSetAlignment UiJustified


------------------------------------------------------------------------------
data UiFitted = UiFitted
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiFitted where
  uiText UiFitted = "fitted"

class UiHasFitted a where
  fitted :: a -> a


------------------------------------------------------------------------------
data UiLeft = UiLeft
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiLeft where
  uiText UiLeft = "left"

class UiHasLeft a where
  uiLeft :: a -> a
  -- Use the ui prefix to not clash with the left function from errors


------------------------------------------------------------------------------
data UiLoading = UiLoading
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiLoading where
  uiText UiLoading = "loading"

class UiHasLoading a where
  loading :: a -> a


------------------------------------------------------------------------------
data UiCompact = UiCompact
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiCompact where
  uiText UiCompact = "compact"

class UiHasCompact a where
  compact :: a -> a


------------------------------------------------------------------------------
data UiToggle = UiToggle
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiToggle where
  uiText UiToggle = "toggle"

class UiHasToggle a where
  uiToggle :: a -> a


------------------------------------------------------------------------------
data UiFluid = UiFluid
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiFluid where
  uiText UiFluid = "fluid"

class UiHasFluid a where
  fluid :: a -> a


------------------------------------------------------------------------------
data UiCircular = UiCircular
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiCircular where
  uiText UiCircular = "circular"

class UiHasCircular a where
  circular :: a -> a


------------------------------------------------------------------------------
data UiError = UiError
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiError where
  uiText UiError = "error"

class UiHasError a where
  hasError :: a -> a


------------------------------------------------------------------------------
data UiBordered = UiBordered
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiBordered where
  uiText UiBordered = "bordered"

class UiHasBordered a where
  bordered :: a -> a


------------------------------------------------------------------------------
data UiTransparent = UiTransparent
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiTransparent where
  uiText UiTransparent = "transparent"

class UiHasTransparent a where
  transparent :: a -> a


------------------------------------------------------------------------------
data UiFloated
  = UiLeftFloated
  | UiRightFloated
  deriving (Eq,Ord,Read,Show,Enum,Bounded)

instance UiClassText UiFloated where
  uiText UiLeftFloated = "left floated"
  uiText UiRightFloated = "right floated"

class UiHasFloated a where
  uiSetFloated :: UiFloated -> a -> a

leftFloated, rightFloated :: UiHasFloated a => a -> a
leftFloated = uiSetFloated UiLeftFloated
rightFloated = uiSetFloated UiRightFloated


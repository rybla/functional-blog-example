module Content where

import Prelude
import Prim hiding (Row)

import Control.Monad.State (class MonadState, State, evalState, get, modify_)
import Data.Array as Array
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.List as List
import Data.Newtype (class Newtype, over)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect.Aff (Aff)
import Halogen (Component, ComponentHTML, Slot)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Record as Record
import Type.Proxy (Proxy(..))

-- =============================================================================

-- A type of kind `ContentKind` can only be introduced as foreign import data.
foreign import data ContentKind :: Type

class Content (c :: ContentKind) where
  renderContent
    :: forall m
     . MonadState ContentEnv m
    => Proxy c
    -> m (Array ContentHTML)

finalizeContent :: forall a. State ContentEnv a -> a
finalizeContent = flip evalState
  { widgetSlotId: WidgetSlotId 0
  }

renderFinalContent :: forall c. Content c => Proxy c -> Array ContentHTML
renderFinalContent = renderContent >>> finalizeContent

-- =============================================================================
-- related types

type ContentHTML = ComponentHTML ContentAction ContentSlots Aff

data ContentAction = Initialize_ContentAction

newtype WidgetSlotId = WidgetSlotId Int

derive instance Newtype WidgetSlotId _
derive newtype instance Show WidgetSlotId
derive newtype instance Eq WidgetSlotId
derive newtype instance Ord WidgetSlotId

type ContentSlots = (widget :: WidgetSlot WidgetSlotId)
_widget = Proxy :: Proxy "widget"

type WidgetSlot slotId = Slot Identity Void slotId

type WidgetComponent = Component Identity Unit Void Aff

-- =============================================================================
-- ContentEnv

type ContentEnv =
  { widgetSlotId :: WidgetSlotId
  }

_widgetSlotId = Proxy :: Proxy "widgetSlotId"

nextWidgetSlotId :: forall m. MonadState ContentEnv m => m WidgetSlotId
nextWidgetSlotId = do
  { widgetSlotId } <- get
  modify_ (Record.modify _widgetSlotId (over WidgetSlotId (_ + 1)))
  pure widgetSlotId

-- =============================================================================

newtype SomeContent = SomeContent (forall r. SomeContentK r -> r)
type SomeContentK r = forall content. Content content => Proxy content -> r

mkSomeContent :: SomeContentK SomeContent
mkSomeContent x = SomeContent \k -> k x

unSomeContent :: forall r. SomeContentK r -> SomeContent -> r
unSomeContent k1 (SomeContent k2) = k2 k1

renderFinalSomeContent :: SomeContent -> Array ContentHTML
renderFinalSomeContent = unSomeContent renderFinalContent

-- =============================================================================
-- Content combinators

foreign import data Literal :: Symbol -> ContentKind

instance IsSymbol s => Content (Literal s) where
  renderContent _ = pure [ HH.span [ HP.class_ (H.ClassName "Literal") ] [ HH.text (reflectSymbol (Proxy :: Proxy s)) ] ]

foreign import data ContentListKind :: Type
foreign import data Cons :: ContentKind -> ContentListKind -> ContentListKind
foreign import data Nil :: ContentListKind

infixr 6 type Cons as :

class ContentList (cs :: ContentListKind) where
  renderContentList
    :: forall m
     . MonadState ContentEnv m
    => Proxy cs
    -> m (List ContentHTML)

newtype SomeContentList = SomeContentList (forall r. SomeContentListK r -> r)
type SomeContentListK r = forall cs. ContentList cs => Proxy cs -> r

mkSomeContentList :: SomeContentListK SomeContentList
mkSomeContentList x = SomeContentList \k -> k x

unSomeContentList :: forall r. SomeContentListK r -> SomeContentList -> r
unSomeContentList k1 (SomeContentList k2) = k2 k1

instance ContentList Nil where
  renderContentList _cs = pure Nil

instance (Content c, ContentList cs) => ContentList (Cons c cs) where
  renderContentList _cs = do
    h <- renderContent (Proxy :: Proxy c)
    hs <- renderContentList (Proxy :: Proxy cs)
    pure ((h # List.fromFoldable) <> hs)

instance (Group group, ContentList cs) => Content (Grouped group cs) where
  renderContent _ = renderGroup (Proxy :: Proxy group) (renderContentList (Proxy :: Proxy cs))

foreign import data Grouped :: GroupKind -> ContentListKind -> ContentKind

foreign import data GroupKind :: Type
foreign import data Column :: GroupKind
foreign import data Row :: GroupKind

class Group (group :: GroupKind) where
  renderGroup
    :: forall m
     . MonadState ContentEnv m
    => Proxy group
    -> m (List ContentHTML)
    -> m (Array ContentHTML)

newtype SomeGroup = SomeGroup (forall r. SomeGroupK r -> r)
type SomeGroupK r = forall group. Group group => Proxy group -> r

mkSomeGroup :: SomeGroupK SomeGroup
mkSomeGroup x = SomeGroup \k -> k x

unSomeGroup :: forall r. SomeGroupK r -> SomeGroup -> r
unSomeGroup k1 (SomeGroup k2) = k2 k1

instance Group Column where
  renderGroup _ mhs = do
    hs <- mhs
    pure [ HH.div [ HP.class_ (H.ClassName "Group_Column") ] (hs # Array.fromFoldable) ]

instance Group Row where
  renderGroup _ mhs = do
    hs <- mhs
    pure [ HH.div [ HP.class_ (H.ClassName "Group_Row") ] (hs # Array.fromFoldable) ]

foreign import data Styled :: StyleKind -> ContentKind -> ContentKind

instance (Content content, Style style) => Content (Styled style content) where
  renderContent _ = renderStyleContent (Proxy :: Proxy style) (renderContent (Proxy :: Proxy content))

foreign import data StyleKind :: Type

class Style (style :: StyleKind) where
  renderStyleContent
    :: forall m
     . MonadState ContentEnv m
    => Proxy style
    -> m (Array ContentHTML)
    -> m (Array ContentHTML)

newtype SomeStyle = SomeStyle (forall r. SomeStyleK r -> r)
type SomeStyleK r = forall style. Style style => Proxy style -> r

mkSomeStyle :: SomeStyleK SomeStyle
mkSomeStyle x = SomeStyle \k -> k x

unSomeStyle :: forall r. SomeStyleK r -> SomeStyle -> r
unSomeStyle k1 (SomeStyle k2) = k2 k1

foreign import data Title :: StyleKind
foreign import data Section :: StyleKind
foreign import data Subsection :: StyleKind
foreign import data Block :: StyleKind
foreign import data Quote :: StyleKind
foreign import data Code :: StyleKind

instance Style Title where
  renderStyleContent _ mh = do
    h <- mh
    pure [ HH.div [ HP.class_ (H.ClassName "Style_Title") ] h ]

instance Style Section where
  renderStyleContent _ mh = do
    h <- mh
    pure [ HH.div [ HP.class_ (H.ClassName "Style_Section") ] h ]

instance Style Subsection where
  renderStyleContent _ mh = do
    h <- mh
    pure [ HH.div [ HP.class_ (H.ClassName "Style_Subsection") ] h ]

instance Style Block where
  renderStyleContent _ mh = do
    h <- mh
    pure [ HH.div [ HP.class_ (H.ClassName "Style_Block") ] h ]

instance Style Quote where
  renderStyleContent _ mh = do
    h <- mh
    pure [ HH.div [ HP.class_ (H.ClassName "Style_Quote") ] h ]

instance Style Code where
  renderStyleContent _ mh = do
    h <- mh
    pure [ HH.div [ HP.class_ (H.ClassName "Style_Code") ] h ]

foreign import data Hole :: ContentKind

instance Content Hole where
  renderContent _ = pure [ HH.div [ HP.class_ (H.ClassName "Hole") ] [] ]


module Content where

import Prelude

import Control.Monad.State (class MonadState, State, evalState)
import Data.Array as Array
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Effect.Aff (Aff)
import Halogen (ComponentHTML, Slot)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import HalogenUtils as HU
import Type.Proxy (Proxy(..))

-- =============================================================================

-- A type of kind `ContentKind` can only be introduced as foreign import data.
foreign import data ContentKind :: Type

class Content (c :: ContentKind) where
  renderContent
    :: forall m slots
     . MonadState ContentState m
    => Proxy c
    -> m (ContentHTML slots)

finalizeContent :: forall a. State ContentState a -> a
finalizeContent = flip evalState
  { widgetSlotId: WidgetSlotId 0
  }

renderFinalContent :: forall c slots. Content c => Proxy c -> ContentHTML slots
renderFinalContent = renderContent >>> finalizeContent

-- =============================================================================
-- related types

type ContentHTML slots = ComponentHTML ContentAction (ContentSlots slots) Aff

type ContentState =
  { widgetSlotId :: WidgetSlotId
  }

data ContentAction
  = Initialize_ContentAction

newtype WidgetSlotId = WidgetSlotId Int

type ContentSlots (slots :: Row Type) = (widget :: WidgetSlot WidgetSlotId | slots)
_widget = Proxy :: Proxy "widget"

type WidgetSlot slotId = Slot Identity Void slotId

newtype SomeContent = SomeContent (forall r. SomeContentK r -> r)
type SomeContentK r = forall c. Content c => Proxy c -> r

-- =============================================================================

mkSomeContent :: SomeContentK SomeContent
mkSomeContent pc = SomeContent \k -> k pc

unSomeContent :: forall r. SomeContentK r -> SomeContent -> r
unSomeContent k1 (SomeContent k2) = k2 k1

renderFinalSomeContent :: forall slots. SomeContent -> ContentHTML slots
renderFinalSomeContent = unSomeContent renderFinalContent

-- =============================================================================
-- Content combinators

foreign import data ContentListKind :: Type
foreign import data ContentKindCons :: ContentKind -> ContentListKind -> ContentListKind
foreign import data ContentKindNil :: ContentListKind

type Nil = ContentKindNil

infixr 6 type ContentKindCons as :

class ContentList (cs :: ContentListKind) where
  renderContentList
    :: forall m slots
     . MonadState ContentState m
    => Proxy cs
    -> m (List (ContentHTML slots))

instance ContentList ContentKindNil where
  renderContentList _cs = pure Nil

instance (Content c, ContentList cs) => ContentList (ContentKindCons c cs) where
  renderContentList _cs = do
    h <- renderContent (Proxy :: Proxy c)
    hs <- renderContentList (Proxy :: Proxy cs)
    pure (h : hs)

foreign import data Column :: ContentListKind -> ContentKind

instance ContentList cs => Content (Column cs) where
  renderContent _ = do
    hs <- renderContentList (Proxy :: Proxy cs)
    pure
      ( HH.div
          [ HP.class_ (H.ClassName "column")
          , HU.style [ "display: flex", "flex-direction: column" ]
          ]
          (hs # Array.fromFoldable)
      )

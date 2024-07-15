module Note where

import Prelude
import Halogen
import Halogen.HTML as HH
import Data.Identity (Identity)
import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))

data Note
  = HTML NoteHTML
  | Include NotePath -- include a note from the registry of pre-defined notes
  | Group GroupMethod (Array Note)

newtype NotePath = NotePath (Array String)

derive newtype instance Show NotePath
derive newtype instance Eq NotePath
derive newtype instance Ord NotePath

data GroupMethod = Column | Row

-- =============================================================================
-- NoteHTML

type NoteHTML = ComponentHTML NoteAction NoteSlots Aff

type NoteState =
  { widgetSlotId :: WidgetSlotId
  }

data NoteAction = Initialize_NoteAction

newtype WidgetSlotId = WidgetSlotId Int

type NoteSlots = (widget :: WidgetSlot WidgetSlotId)
_widget = Proxy :: Proxy "widget"

type WidgetSlot slotId = Slot Identity Void slotId


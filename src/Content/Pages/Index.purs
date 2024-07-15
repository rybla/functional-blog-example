--props { "public_path": "index" }
module Content.Pages.Index where

import Prelude

import Content (class Content, ContentKind, WidgetComponent, _widget, mkSomeContent, nextWidgetSlotId)
import Debug as Debug
import Halogen (defaultEval, mkComponent, mkEval)
import Halogen.HTML as HH
import HalogenUtils as HU
import Page as Page
import Type.Proxy (Proxy(..))

static_content = Page.static_content spec
start_client = Page.start_client spec

-- =============================================================================

spec :: Page.Spec
spec = Page.Spec
  { title: "Index"
  , static_content: "This is the index of the entire website."
  , content: mkSomeContent (Proxy :: Proxy Index)
  }

foreign import data Index :: ContentKind

instance Content Index where
  renderContent _ = do
    Debug.traceM "render Index"
    widgetSlotId <- nextWidgetSlotId
    pure (HH.slot_ _widget widgetSlotId mainWidget unit)

mainWidget :: WidgetComponent
mainWidget = mkComponent { initialState, eval, render }
  where
  initialState _input =
    { query_string: ""
    }

  eval = mkEval defaultEval

  render _state =
    HH.div
      [ HU.style [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
      [ HH.div [] [ HH.input [] ]
      , HH.div [] [ HH.text "<result>" ]
      ]


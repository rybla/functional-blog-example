--props { "public_path": "index" }
module Content.Pages.Index where

import Prelude

import Content (class Content, ContentKind, WidgetComponent, _widget, mkSomeContent, nextWidgetSlotId)
import Data.Maybe (Maybe(..))
import Effect.Class.Console as Console
import Halogen (RefLabel(..), defaultEval, mkComponent, mkEval, modify_)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenUtils as HU
import Page as Page
import Type.Proxy (Proxy(..))
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

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
    widgetSlotId <- nextWidgetSlotId
    pure (HH.slot_ _widget widgetSlotId mainWidget unit)

data Action
  = Initialize
  | OnValueChangeInput String
  | OnKeyUpInput KeyboardEvent

mainWidget :: WidgetComponent
mainWidget = mkComponent { initialState, eval, render }
  where
  initialState _input =
    { query_string: ""
    }

  eval = mkEval defaultEval { initialize = Just Initialize, handleAction = handleAction }

  handleAction = case _ of
    Initialize -> do
      Console.log "initialize"
    OnValueChangeInput string -> do
      modify_ _ { query_string = string }
    OnKeyUpInput _event -> do
      -- inputElem <- getInputElem
      -- value <- inputElem # HTMLInputElement.value # liftEffect
      -- modify_ _ { query_string = value }
      pure unit

  inputRefLabel = RefLabel "input"
  -- getInputElem = H.getHTMLElementRef inputRefLabel
  --   # bindFlipped (maybe (throwError (error "input element not found")) pure)
  --   # map HTMLInputElement.fromHTMLElement
  --   # bindFlipped (maybe (throwError (error "input element wasn't an HTMLInputElement")) pure)

  render state =
    HH.div
      [ HU.style [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
      [ HH.div [] [ HH.input [ HP.ref inputRefLabel, HE.onValueChange OnValueChangeInput, HE.onKeyUp OnKeyUpInput ] ]
      , HH.div [] [ HH.text ("query_string: " <> state.query_string) ]
      , HH.div [] [ HH.text "<result>" ]
      ]


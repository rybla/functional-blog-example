--props { "public_path": "index" }
module Content.Pages.Index where

import Prelude

import Content (class Content, ContentAction, ContentKind, WidgetComponent, _widget, mkSomeContent, nextWidgetSlotId)
import Control.Monad.State.Class (put)
import Data.Argonaut (JsonDecodeError)
import Data.Argonaut as Argonaut
import Data.Argonaut.Decode as Argonaut.Decode
import Data.Argonaut.Encode as Argonaut.Encode
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen (Component, RefLabel(..), defaultEval, mkComponent, mkEval, modify_)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenUtils as HU
import Page as Page
import SomeContentBuilder (SomeContentBuilder(..), SomeContentListBuilder(..), SomeGroupBuilder(..), renderSomeContentBuilder, (:))
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
    pure (HH.slot_ _widget widgetSlotId mainComponent unit)

-- =============================================================================

data Action
  = Initialize
  | OnValueChangeInput String
  | OnKeyUpInput KeyboardEvent

mainComponent :: WidgetComponent
mainComponent = mkComponent { initialState, eval, render }
  where
  initialState _input =
    { query_string: ""
    , mb_err_content_builder: Nothing :: Maybe (JsonDecodeError \/ SomeContentBuilder)
    }

  eval = mkEval defaultEval { initialize = Just Initialize, handleAction = handleAction }

  handleAction = case _ of
    Initialize -> do
      pure unit
    OnValueChangeInput query_string -> do
      modify_ _ { query_string = query_string }
      let
        query_json = Argonaut.fromString query_string
      Console.log (Argonaut.stringify query_json)
      let
        err_content_builder :: _ \/ SomeContentBuilder
        err_content_builder = query_string # Argonaut.Decode.fromJsonString
      modify_ _ { mb_err_content_builder = Just err_content_builder }
      pure unit
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

  -- TODO: instead of string input, create a little structure editor for constructing `SomeContentBuilder`
  render state =
    HH.div
      [ HU.style [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
      [ HH.input [ HP.ref inputRefLabel, HE.onValueChange OnValueChangeInput, HE.onKeyUp OnKeyUpInput ]
      -- , HH.div [] [ HH.text ("query_string: " <> state.query_string) ]
      , HH.div
          [ HU.style [ "padding: 0.5em" ] ]
          [ HH.div [] [ HH.text ("example: " <> (Include "NoteA" # Argonaut.Encode.toJsonString)) ]
          , HH.div [] [ HH.text ("example: " <> (Include "NoteB" # Argonaut.Encode.toJsonString)) ]
          , HH.div [] [ HH.text ("example: " <> (Grouped Column (Include "NoteA" : Include "NoteB" : Nil) # Argonaut.Encode.toJsonString)) ]
          ]
      , HH.div
          [ HU.style [ "padding: 0.5em", "box-shadow: 0 0 0.5em 0" ] ]
          [ case state.mb_err_content_builder of
              Nothing -> HH.span_ [ HH.text "<Nothing>" ]
              Just (Left err) -> HH.span_ [ HH.text ("[JsonDecodeError] " <> show err) ]
              Just (Right content_builder) -> HH.slot_ (Proxy :: Proxy "content") unit subcontentComponent { content_builder }
          ]
      ]

-- =============================================================================

data SubcontentAction
  = ReceiveSubcontentAction SubcontentInput
  | SubcontentAction ContentAction

type SubcontentInput =
  { content_builder :: SomeContentBuilder
  }

subcontentComponent :: forall query output. Component query SubcontentInput output Aff
subcontentComponent = mkComponent { initialState, eval, render }
  where

  initialState input =
    { content_builder: input.content_builder
    }

  eval = mkEval defaultEval
    { receive = Just <<< ReceiveSubcontentAction
    , handleAction = case _ of
        ReceiveSubcontentAction input -> put input
        SubcontentAction _action -> pure unit
    }

  render state = renderSomeContentBuilder state.content_builder # bimap (map SubcontentAction) SubcontentAction

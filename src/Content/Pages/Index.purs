--props { "public_path": "index" }
module Content.Pages.Index where

import Content
import Prelude
import SomeContentBuilder

import Control.Monad.State.Class (get, modify_, put)
import Data.Argonaut (JsonDecodeError)
import Data.Argonaut as Argonaut
import Data.Argonaut.Decode as Argonaut.Decode
import Data.Argonaut.Encode as Argonaut.Encode
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either(..), isLeft)
import Data.Either.Nested (type (\/))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Debug as Debug
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Halogen (Component, RefLabel(..), defaultEval, mkComponent, mkEval, modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenUtils as HU
import Page as Page
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))
import Unsafe as Unsafe
import Web.Event.Event as Event
import Web.HTML.HTMLSelectElement as HTMLSelectElement
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
  | OnQueryChange SomeContentBuilder
  | SetShowEditor_Action Boolean

mainComponent :: WidgetComponent
mainComponent = mkComponent { initialState, eval, render }
  where
  initialState _input =
    { show_editor: false
    , mb_err_content_builder: Nothing :: Maybe (JsonDecodeError \/ SomeContentBuilder)
    }

  eval = mkEval defaultEval { initialize = Just Initialize, handleAction = handleAction }

  handleAction = case _ of
    Initialize -> do
      pure unit
    OnQueryChange content_builder -> do
      modify_ _ { mb_err_content_builder = Just (Right content_builder) }
      pure unit -- TODO
    SetShowEditor_Action show_editor -> do
      modify_ _ { show_editor = show_editor }

  render state =
    HH.div
      [ HU.style [ "width: 100vw", "height: 100vh", "display: flex", "flex-direction: column" ] ]
      [ HH.div
          [ HE.onClick (\_ -> SetShowEditor_Action (not state.show_editor))
          , HU.style [ "align-content: center", "text-align: center" ]
          ]
          [ HH.button [] [ HH.text if state.show_editor then "hide editor" else "show editor" ] ]
      , HH.div
          [ HU.style ([ [ "max-height: 50%", "overflow-y: scroll" ], if state.show_editor then [] else [ "display: none" ] ] # Array.fold) ]
          [ HH.div
              [ HU.style [ "padding: 0.5em" ] ]
              [ HH.slot (Proxy :: Proxy "query") unit editorComponent { content: Grouped Column (Include "NoteA" : Include "NoteB" : Styled Quote Hole : Nil) } case _ of
                  UpdateEditorOutput content -> OnQueryChange content
              ]
          ]
      , HH.div
          [ HU.style [ "flex-grow: 1", "overflow-y: scroll" ] ]
          [ HH.div
              [ HU.style [ "padding: 0.5em" ] ]
              [ case state.mb_err_content_builder of
                  Nothing -> HH.span_ [ HH.text "<Nothing>" ]
                  Just (Left err) -> HH.span_ [ HH.text ("[JsonDecodeError] " <> show err) ]
                  Just (Right content) -> HH.slot_ (Proxy :: Proxy "content") unit subcontentComponent { content }
              ]
          ]
      ]

-- =============================================================================
-- A little structure editor for building a `SomeContentBuilder`.

type EditorInput =
  { content :: SomeContentBuilder
  }

data EditorOutput = UpdateEditorOutput SomeContentBuilder

data EditorAction
  = Initialize_EditorAction
  | SetSomeContentBuilder_EditorAction SomeContentBuilder
  | Pass_EditorAction

editorComponent :: forall query. Component query EditorInput EditorOutput Aff
editorComponent = mkComponent { initialState, eval, render }
  where
  initialState input =
    { content: input.content
    }

  eval = mkEval defaultEval { initialize = Just Initialize_EditorAction, handleAction = handleAction }

  handleAction = case _ of
    Initialize_EditorAction -> do
      { content } <- get
      H.raise (UpdateEditorOutput content)
    SetSomeContentBuilder_EditorAction content -> do
      modify_ _ { content = content }
      H.raise (UpdateEditorOutput content)
    Pass_EditorAction -> pure unit

  render state = HH.div [] [ render_SomeContentBuilder identity state.content ]

  padding_small = "padding: 0.5em"
  paddingLeft_small = "padding-left: 0.5em"
  boxshadow_small = "box-shadow: 0 0 0 0.1em"

  render_SomeContentBuilder :: (SomeContentBuilder -> SomeContentBuilder) -> SomeContentBuilder -> _
  render_SomeContentBuilder wrap = case _ of
    Include name ->
      let
        names = namedSomeContent # Map.keys # Array.fromFoldable
      in
        HH.div
          [ HU.style [ boxshadow_small, padding_small, "display: flex", "flex-direction: row", "gap: 0.5em" ] ]
          [ render_controls wrap
          , HH.select
              [ HE.onChange
                  ( \event ->
                      let
                        i = event # Event.target # Unsafe.fromJust # HTMLSelectElement.fromEventTarget # Unsafe.fromJust # HTMLSelectElement.value # unsafePerformEffect # Int.fromString # Unsafe.fromJust
                        name' = names Array.!! i # Unsafe.fromJust
                      in
                        SetSomeContentBuilder_EditorAction (wrap (Include name'))
                  )
              ]
              ( names # mapWithIndex \i name' ->
                  HH.option
                    [ HP.selected (name == name'), HP.value (show i) ]
                    [ HH.text name ]
              )
          ]
    Styled style content ->
      HH.div
        [ HU.style [ boxshadow_small, padding_small, "display: flex", "flex-direction: row", "gap: 0.5em" ] ]
        [ render_controls wrap
        , HH.div
            [ HU.style [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
            [ render_SomeStyleBuilder (wrap <<< \style' -> Styled style' content) style
            , render_SomeContentBuilder (wrap <<< \content' -> Styled style content') content
            ]
        ]

    Grouped group list ->
      HH.div
        [ HU.style [ boxshadow_small, padding_small, "display: flex", "flex-direction: row", "gap: 0.5em" ] ]
        [ render_controls wrap
        , HH.div
            [ HU.style [ "display: flex", "flex-direction: column", "gap: 0.5em", paddingLeft_small, "border-left: 0.1em dashed black" ] ]
            [ render_SomeGroupBuilder (wrap <<< \group' -> Grouped group' list) group
            , render_SomeContentListBuilder (wrap <<< \list' -> Grouped group list') list
            ]
        ]
    Hole ->
      let
        options =
          [ Left unit ] <>
            ( Right <$>
                [ "Grouped" /\ Grouped Column Nil
                , "Styled" /\ Styled Quote Hole
                , "Named" /\ Include "NoteA"
                ]
            )
      in
        HH.div
          [ HU.style [ boxshadow_small, padding_small ] ]
          [ HH.select
              [ HE.onChange
                  ( \event ->
                      let
                        i = event # Event.target # Unsafe.fromJust # HTMLSelectElement.fromEventTarget # Unsafe.fromJust # HTMLSelectElement.value # unsafePerformEffect # Int.fromString # Unsafe.fromJust
                      in
                        case options Array.!! i # Unsafe.fromJust of
                          Left _ -> Pass_EditorAction
                          Right (_ /\ content') -> SetSomeContentBuilder_EditorAction (wrap content')
                  )
              ]
              ( options # mapWithIndex \i option -> HH.option [ HP.selected (isLeft option), HP.value (show i) ]
                  [ HH.text case option of
                      Left _ -> "hole"
                      Right (label /\ _) -> label
                  ]
              )
          ]

  render_SomeStyleBuilder wrap style =
    let
      styles = [ Quote, Code ]
    in
      HH.div
        []
        [ HH.select
            [ HE.onChange
                ( \event ->
                    let
                      i = event # Event.target # Unsafe.fromJust # HTMLSelectElement.fromEventTarget # Unsafe.fromJust # HTMLSelectElement.value # unsafePerformEffect # Int.fromString # Unsafe.fromJust
                      style' = styles Array.!! i # Unsafe.fromJust
                    in
                      SetSomeContentBuilder_EditorAction (wrap style')
                )
            ]
            (styles # mapWithIndex \i style' -> HH.option [ HP.selected (style == style'), HP.value (show i) ] [ HH.text (show style') ])
        ]

  render_SomeGroupBuilder wrap group =
    let
      groups = [ Row, Column ]
    in
      HH.div
        []
        [ HH.select
            [ HE.onChange
                ( \event ->
                    let
                      i = event # Event.target # Unsafe.fromJust # HTMLSelectElement.fromEventTarget # Unsafe.fromJust # HTMLSelectElement.value # unsafePerformEffect # Int.fromString # Unsafe.fromJust
                      group' = groups Array.!! i # Unsafe.fromJust
                    in
                      SetSomeContentBuilder_EditorAction (wrap group')
                )
            ]
            (groups # mapWithIndex \i group' -> HH.option [ HP.selected (group == group'), HP.value (show i) ] [ HH.text (show group') ])
        ]

  render_SomeContentListBuilder wrap = case _ of
    Nil ->
      HH.div
        [ HU.style [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
        [ HH.div
            [ HU.style [ "display: flex", "flex-direction: row", "gap: 0.5em" ] ]
            [ HH.button [ HE.onClick (\_ -> SetSomeContentBuilder_EditorAction (wrap (Cons Hole Nil))) ] [ HH.text "+" ]
            ]
        , HH.div
            [ HU.style [ boxshadow_small, padding_small, "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
            [ HH.div
                [ HU.style [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
                [ HH.div [] [ HH.text "Nil" ]
                ]
            ]
        ]

    Cons content list ->
      HH.div
        [ HU.style [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
        [ HH.div
            [ HU.style [ "display: flex", "flex-direction: row", "gap: 0.5em" ] ]
            [ HH.button [ HE.onClick (\_ -> SetSomeContentBuilder_EditorAction (wrap (Cons Hole (Cons content list)))) ] [ HH.text "➕" ]
            , HH.button [ HE.onClick (\_ -> SetSomeContentBuilder_EditorAction (wrap list)) ] [ HH.text "➖" ]
            ]
        , HH.div
            [ HU.style [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
            [ render_SomeContentBuilder (wrap <<< \content' -> Cons content' list) content
            , render_SomeContentListBuilder (wrap <<< \list' -> Cons content list') list
            ]
        ]

  render_controls wrap =
    HH.div
      []
      [ HH.button [ HE.onClick (\_ -> SetSomeContentBuilder_EditorAction (wrap Hole)) ] [ HH.text "❌" ] ]

-- =============================================================================

data SubcontentAction
  = ReceiveSubcontentAction SubcontentInput
  | SubcontentAction ContentAction

type SubcontentInput =
  { content :: SomeContentBuilder
  }

subcontentComponent :: forall query output. Component query SubcontentInput output Aff
subcontentComponent = mkComponent { initialState, eval, render }
  where

  initialState input =
    { content: input.content
    }

  eval = mkEval defaultEval
    { receive = Just <<< ReceiveSubcontentAction
    , handleAction = case _ of
        ReceiveSubcontentAction input -> put input
        SubcontentAction _action -> pure unit
    }

  render state = renderSomeContentBuilder state.content # bimap (map SubcontentAction) SubcontentAction

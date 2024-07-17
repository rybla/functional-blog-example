--props { "public_path": "index" }
module Content.Pages.Index where

import Content
import Prelude
import SomeContentBuilder

import Content.Notes (namedSomeContent)
import Control.Monad.State.Class (get, put)
import Data.Argonaut (JsonDecodeError)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either, isLeft, isRight)
import Data.Either.Nested (type (\/))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, error, throwError)
import Effect.Class.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Halogen (Component, defaultEval, liftEffect, mkComponent, mkEval, modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenUtils as HU
import JSURI (decodeURIComponent, encodeURIComponent)
import Page as Page
import Type.Proxy (Proxy(..))
import Unsafe as Unsafe
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.HTMLSelectElement as HTMLSelectElement
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.URL.URLSearchParams as URLSearchParams

static_content = Page.static_content spec
start_client = Page.start_client spec

-- =============================================================================

spec :: Page.Spec
spec = Page.Spec
  { title: "Index"
  , static_content: "This is the index of the entire website."
  , stylesheet_hrefs: [ "./main.css" ]
  , content: mkSomeContent (Proxy :: Proxy Index)
  }

foreign import data Index :: ContentKind

instance Content Index where
  renderContent _ = do
    widgetSlotId <- nextWidgetSlotId
    pure [ HH.slot_ _widget widgetSlotId mainComponent unit ]

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
    , mb_err_content_builder: Nothing :: Maybe (String \/ SomeContentBuilder)
    }

  eval = mkEval defaultEval { initialize = Just Initialize, handleAction = handleAction }

  handleAction = case _ of
    Initialize -> do
      window <- HTML.window # liftEffect
      location <- window # Window.location # liftEffect
      search <- location # Location.search # liftEffect
      Console.log (show { search })
      usp <- URLSearchParams.fromString search # pure
      usp # URLSearchParams.get "content" # case _ of
        Nothing -> pure unit
        Just content_string_ -> do
          content_string <- content_string_ # decodeURIComponent # maybe (throwError (error "railed to decodeURIComponent")) pure
          Console.log (show { content_string })
          case decodeMu content_string :: _ SomeContentBuilder of
            Left err -> do
              Console.log "decoded failurely"
              modify_ _ { mb_err_content_builder = Just (Left (show err)) }
            Right content -> do
              Console.log "decoded successfully"
              modify_ _ { mb_err_content_builder = Just (Right content) }
    OnQueryChange content -> do
      modify_ _ { mb_err_content_builder = Just (Right content) }
      pure unit -- TODO
    SetShowEditor_Action show_editor -> do
      modify_ _ { show_editor = show_editor }

  render state =
    HH.div
      [ HU.style [ "height: 100vh", "display: flex", "flex-direction: column" ] ]
      [ HH.div
          [ HE.onClick (\_ -> SetShowEditor_Action (not state.show_editor))
          , HU.style [ "padding: 0.5em", "align-content: center", "text-align: center" ]
          ]
          [ HH.button [] [ HH.text if state.show_editor then "hide editor" else "show editor" ] ]
      , HH.div
          [ HU.style (if state.show_editor then [ "max-height: 50vh", "overflow-y: scroll" ] else [ "display: none" ]) ]
          [ HH.div
              [ HU.style [ "padding: 0.5em" ] ]
              [ HH.slot (Proxy :: Proxy "query") unit editorComponent
                  { content:
                      case state.mb_err_content_builder of
                        Just (Right content) -> content
                        _ -> Hole
                  }
                  case _ of
                    UpdateEditorOutput content -> OnQueryChange content
              ]
          ]
      , HH.div
          [ HU.style ((if (state.mb_err_content_builder # maybe false (either (const false) (const true))) then [] else [ "max-height: 40vh" ]) <> [ "overflow-y: scroll" ]) ]
          [ HH.div
              [ HU.style [ "padding: 0.5em" ] ]
              [ case state.mb_err_content_builder of
                  Nothing -> HH.span_ [ HH.text "<Nothing>" ]
                  Just (Left err) -> HH.span_ [ HH.text (show err) ]
                  Just (Right content) ->
                    HH.div
                      [ HU.style [ "display: flex", "flex-direction: column", "gap: 1em" ] ]
                      [ let
                          enc = content # encodeMu
                          href = "/?content=" <> (enc # encodeURIComponent # Unsafe.fromJust)
                          href_len = href # String.length
                        in
                          HH.div
                            [ HU.style [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
                            ( ( [ "Google Chrome" /\ 2048
                                , "Mozilla Firefox" /\ 65536
                                , "Internet Explorer" /\ 2083
                                , "Safari" /\ 80000
                                , "Opera" /\ 190000
                                ]
                                  # Array.filter (\(_ /\ n) -> href_len > n)
                                  # map (\(browser_name /\ n) -> HH.div [ HU.style [ "color: red" ] ] [ HH.text ("This URL's length (" <> show href_len <> ") is too long (> " <> show n <> ") for " <> browser_name) ])
                              )
                                <>
                                  [ HH.div
                                      []
                                      [ HH.a
                                          [ HU.style [ "font-family: monospace" ]
                                          , HP.href href
                                          ]
                                          [ HH.text enc ]
                                      ]
                                  ]
                            )
                      , HH.slot_ (Proxy :: Proxy "content") unit subcontentComponent { content }
                      ]
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
  | Receive_EditorAction EditorInput
  | SetSomeContentBuilder_EditorAction SomeContentBuilder
  | Pass_EditorAction

editorComponent :: forall query. Component query EditorInput EditorOutput Aff
editorComponent = mkComponent { initialState, eval, render }
  where
  initialState input =
    { content: input.content
    }

  eval = mkEval defaultEval { initialize = Just Initialize_EditorAction, receive = Just <<< Receive_EditorAction, handleAction = handleAction }

  handleAction = case _ of
    Initialize_EditorAction -> do
      -- { content } <- get
      -- H.raise (UpdateEditorOutput content)
      pure unit
    Receive_EditorAction input -> do
      H.put (initialState input)
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
    Literal str ->
      HH.div
        [ HU.style [ boxshadow_small, padding_small, "display: flex", "flex-direction: row", "gap: 0.5em" ] ]
        [ render_controls wrap
        , HH.textarea
            [ HP.value str
            , HE.onValueChange (\str' -> SetSomeContentBuilder_EditorAction (wrap (Literal str')))
            ]
        ]
    Named name ->
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
                        SetSomeContentBuilder_EditorAction (wrap (Named name'))
                  )
              ]
              ( names # mapWithIndex \i name' ->
                  HH.option
                    [ HP.selected (name == name'), HP.value (show i) ]
                    [ HH.text name' ]
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
                [ "Literal" /\ Literal ""
                , "Named" /\ Named "loremIpsum"
                , "Grouped" /\ Grouped Column Nil
                , "Styled" /\ Styled Quote Hole
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
                      Left _ -> ""
                      Right (label /\ _) -> label
                  ]
              )
          ]

  render_SomeStyleBuilder wrap style =
    let
      styles =
        [ Title
        , Section
        , Subsection
        , Block
        , Quote
        , Code
        ]
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
            [ HH.button [ HE.onClick (\_ -> SetSomeContentBuilder_EditorAction (wrap (Cons Hole Nil))) ] [ HH.text "➕" ]
            ]
        -- , HH.div
        --     [ HU.style [ boxshadow_small, padding_small, "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
        --     [ HH.div
        --         [ HU.style [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
        --         [ HH.div [] [ HH.text "Nil" ]
        --         ]
        --     ]
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

  render state =
    HH.div
      []
      (renderSomeContentBuilder state.content # map (bimap (map SubcontentAction) SubcontentAction))

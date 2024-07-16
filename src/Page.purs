module Page where

import Prelude

import Content (ContentAction(..), SomeContent(..), renderFinalSomeContent, unSomeContent)
import Data.Array as Array
import Data.Identity (Identity)
import Data.Maybe (Maybe(..), maybe')
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Halogen (Component, defaultEval, liftEffect, mkComponent, mkEval)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML as HTML
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)
import Web.HTML.Window as Window

-- =============================================================================

newtype Spec = Spec
  { title :: String
  , static_content :: String
  , stylesheet_hrefs :: Array String
  , content :: SomeContent
  }

-- =============================================================================
-- static_content

static_content :: Spec -> String
static_content (Spec spec) =
  """
<!DOCTYPE html>
<html lang="en">

<body>
<div id="static_content">"""
    <> spec.static_content
    <>
      """</div>
</body>

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>"""
    <> spec.title
    <>
      """</title>
  <style>
    html, body { margin: 0; padding: 0 }
  </style>
  <script src="main.js"></script>
  """
    <> (spec.stylesheet_hrefs # map (\href -> "<link rel=\"stylesheet\" href=\"" <> href <> "\">") # Array.intercalate "\n  ")
    <>
      """
</head>

</html>
"""

-- =============================================================================
-- start_client

start_client :: Spec -> Effect Unit
start_client (Spec spec) = runHalogenAff do runUI component {} =<< awaitBody
  where
  component = mkComponent { initialState, eval, render }
    where
    initialState _input = {}

    eval = mkEval defaultEval { initialize = Just Initialize_ContentAction, handleAction = handleAction }

    handleAction = case _ of
      Initialize_ContentAction -> do
        -- set title of document
        (setTitle spec.title =<< document =<< window) # liftEffect
        -- remove static_content
        liftEffect do
          document <- Window.document =<< HTML.window
          body <- (HTMLDocument.body =<< Window.document =<< HTML.window) >>= maybe' (const (throwError (error "no body"))) pure
          e <- getElementById "static_content" (document # HTMLDocument.toNonElementParentNode) >>= maybe' (const (throwError (error "couldn't find #static_content"))) pure
          Node.removeChild (e # Element.toNode) (body # HTMLElement.toNode)
        pure unit

    render _state =
      HH.div
        [ HP.class_ (H.ClassName "Page") ]
        (spec.content # renderFinalSomeContent)

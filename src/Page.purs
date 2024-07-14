module Page where

import Prelude

import Data.Identity (Identity)
import Data.Maybe (Maybe(..), maybe')
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Halogen (Component, defaultEval, liftEffect, mkComponent, mkEval)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
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
  , component :: Component Identity Unit Void Aff
  }

-- =============================================================================
-- static_content

static_content :: Spec -> String
static_content (Spec spec) =
  """
<!DOCTYPE html>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>""" <> spec.title
    <>
      """</title>
  <script src="main.js"></script>
</head>

<body>
<div id="static_content">"""
    <> spec.static_content
    <>
      """</div>
</body>

</html>
"""

-- =============================================================================
-- start_client

data Action = Initialize

start_client :: Spec -> Effect Unit
start_client (Spec spec) = runHalogenAff do runUI component {} =<< awaitBody
  where
  component = mkComponent { initialState, eval, render }
    where
    initialState _input = {}

    eval = mkEval defaultEval { initialize = Just Initialize, handleAction = handleAction }

    handleAction = case _ of
      Initialize -> do
        -- set title of document
        (setTitle spec.title =<< document =<< window) # liftEffect
        -- remove static_content
        liftEffect do
          document <- Window.document =<< HTML.window
          body <- (HTMLDocument.body =<< Window.document =<< HTML.window) >>= maybe' (const (throwError (error "no body"))) pure
          e <- getElementById "static_content" (document # HTMLDocument.toNonElementParentNode) >>= maybe' (const (throwError (error "couldn't find #static_content"))) pure
          Node.removeChild (e # Element.toNode) (body # HTMLElement.toNode) 
        pure unit

    render _state = HH.slot_ (Proxy :: Proxy "component") unit spec.component unit

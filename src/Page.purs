module Page where

import Prelude

import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen (Component, defaultEval, liftEffect, mkComponent, mkEval)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument (setTitle)
import Web.HTML.Window (document)

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
  """
    <> spec.static_content
    <>
      """
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
        (setTitle spec.title =<< document =<< window) # liftEffect
        pure unit

    render _state = HH.slot_ (Proxy :: Proxy "component") unit spec.component unit

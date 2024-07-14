--props { "public_path": "example2" }
module Content.Pages.Example2 where

import Prelude

import Halogen (defaultEval, mkComponent, mkEval)
import Halogen.HTML as HH
import Page as Page

-- =============================================================================

static_content = Page.static_content spec
start_client = Page.start_client spec

-- =============================================================================

spec :: Page.Spec
spec = Page.Spec
  { title: "Example2"
  , static_content: "This is a placeholder for Example2."
  , component
  }

component = mkComponent { initialState, eval, render }
  where
  initialState _input = {}

  eval = mkEval defaultEval

  render _state = HH.div [] [ HH.text "This is the page Example2" ]


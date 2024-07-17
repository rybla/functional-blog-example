--props { "public_path": "example1" }
module Content.Pages.Example1 where

import Content (Hole, mkSomeContent)
import Page as Page
import Type.Proxy (Proxy(..))

static_content = Page.static_content spec
start_client = Page.start_client spec

-- =============================================================================

spec :: Page.Spec
spec = Page.Spec
  { title: "Example1"
  , static_content: "This is a placeholder."
  , stylesheet_hrefs: [ "../main.css" ]
  , content: mkSomeContent
      ( Proxy
          :: Proxy
               Hole -- NoteA
      )
  }

--props { "public_path": "example2" }
module Content.Pages.Example2 where

import Content
import Content.Notes.NoteA (NoteA)
import Content.Notes.NoteB (NoteB)
import Page as Page
import Type.Proxy (Proxy(..))

static_content = Page.static_content spec
start_client = Page.start_client spec

-- =============================================================================

spec :: Page.Spec
spec = Page.Spec
  { title: "Example2"
  , static_content: "This is a placeholder for Example2."
  , stylesheet_hrefs: [ "../main.css" ]
  , content: mkSomeContent
      ( Proxy
          :: Proxy
               ( Grouped Column
                   ( NoteA
                       : NoteB
                       : Nil
                   )
               )
      )
  }


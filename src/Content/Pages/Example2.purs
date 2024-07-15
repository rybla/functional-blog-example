--props { "public_path": "example2" }
module Content.Pages.Example2 where

import Prelude

import Content
import Content.Notes.NoteA (NoteA)
import Content.Notes.NoteB (NoteB)
import Data.Identity (Identity)
import Halogen (defaultEval, mkComponent, mkEval)
import Halogen.HTML as HH
import Page as Page
import Type.Proxy (Proxy(..))

-- =============================================================================

static_content = Page.static_content spec
start_client = Page.start_client spec

-- =============================================================================

spec :: Page.Spec
spec = Page.Spec
  { title: "Example2"
  , static_content: "This is a placeholder for Example2."
  , content: mkSomeContent (Proxy :: Proxy (Column (NoteA : NoteB : Nil)))
  }



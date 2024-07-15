module Content.Notes.NoteB where

import Content
import Prelude

import Halogen.HTML as HH

foreign import data NoteB :: ContentKind

instance Content NoteB where
  renderContent _ = pure (HH.div [] [ HH.text "This is NoteB." ])

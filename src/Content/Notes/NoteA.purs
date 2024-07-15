module Content.Notes.NoteA where

import Content
import Prelude

import Halogen.HTML as HH

foreign import data NoteA :: ContentKind

instance Content NoteA where
  renderContent _ = pure (HH.div [] [ HH.text "This is NoteA." ])

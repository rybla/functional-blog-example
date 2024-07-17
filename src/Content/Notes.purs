module Content.Notes where

import Content (class Content, ContentKind, SomeContent, mkSomeContent)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Type.Prelude (Proxy(..))
import Content.Notes.LoremIpsum as LoremIpsum
import Content.Notes.NoteA as NoteA
import Content.Notes.NoteB as NoteB



foreign import data Content_noteB :: ContentKind
instance Content Content_noteB where
  renderContent _ = NoteB.noteB


foreign import data Content_noteA :: ContentKind
instance Content Content_noteA where
  renderContent _ = NoteA.noteA


foreign import data Content_loremIpsum_long :: ContentKind
instance Content Content_loremIpsum_long where
  renderContent _ = LoremIpsum.loremIpsum_long


foreign import data Content_loremIpsum :: ContentKind
instance Content Content_loremIpsum where
  renderContent _ = LoremIpsum.loremIpsum



namedSomeContent :: Map String SomeContent
namedSomeContent = Map.fromFoldable
  [ "noteB" /\ mkSomeContent (Proxy :: Proxy Content_noteB)
  , "noteA" /\ mkSomeContent (Proxy :: Proxy Content_noteA)
  , "loremIpsum_long" /\ mkSomeContent (Proxy :: Proxy Content_loremIpsum_long)
  , "loremIpsum" /\ mkSomeContent (Proxy :: Proxy Content_loremIpsum)
  ]
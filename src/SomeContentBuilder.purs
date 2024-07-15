module SomeContentBuilder where

import Content
import Prelude
import Prim hiding (Row)

import Content.Notes.NoteA (NoteA)
import Content.Notes.NoteB (NoteB)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafeCrashWith)
import Type.Prelude (Proxy(..))

-- =============================================================================

type Result = Either String

-- =============================================================================
-- SomeContentBuilder

data SomeContentBuilder
  = Named String
  | Styled SomeStyleBuilder SomeContentBuilder
  | Grouped SomeGroupBuilder SomeContentListBuilder

derive instance Generic SomeContentBuilder _

instance EncodeJson SomeContentBuilder where
  encodeJson x = genericEncodeJson x

instance DecodeJson SomeContentBuilder where
  decodeJson x = genericDecodeJson x

fromSomeContentBuilder :: SomeContentBuilder -> Result SomeContent
fromSomeContentBuilder = case _ of
  Named name -> fromNamedSomeContentBuilder name
  Grouped group_builder list_builder -> do
    some_group <- group_builder # fromSomeGroupBuilder
    some_list <- list_builder # fromSomeContentListBuilder
    some_group # unSomeGroup \group ->
      some_list # unSomeContentList \list ->
        pure (mkSomeContent (proxyGrouped group list))
  Styled style_builder content_builder -> do
    some_style <- style_builder # fromSomeStyleBuilder
    some_content <- content_builder # fromSomeContentBuilder
    some_content # unSomeContent \content ->
      some_style # unSomeStyle \style ->
        pure (mkSomeContent (proxyStyled style content))

proxyGrouped :: forall group list. Proxy group -> Proxy list -> Proxy (Grouped group list)
proxyGrouped _ _ = Proxy

proxyStyled :: forall style content. Proxy style -> Proxy content -> Proxy (Styled style content)
proxyStyled _ _ = Proxy

-- =============================================================================
-- SomeContentListBuilder

data SomeContentListBuilder
  = Nil
  | Cons SomeContentBuilder SomeContentListBuilder

derive instance Generic SomeContentListBuilder _

instance EncodeJson SomeContentListBuilder where
  encodeJson x = genericEncodeJson x

instance DecodeJson SomeContentListBuilder where
  decodeJson x = genericDecodeJson x

fromSomeContentListBuilder :: SomeContentListBuilder -> Result SomeContentList
fromSomeContentListBuilder = case _ of
  Nil -> pure (mkSomeContentList (Proxy :: Proxy Nil))
  Cons content_builder list_builder -> do
    some_content <- content_builder # fromSomeContentBuilder
    some_list <- list_builder # fromSomeContentListBuilder
    some_content # unSomeContent \content ->
      some_list # unSomeContentList \list ->
        pure (mkSomeContentList (proxyCons content list))

proxyCons :: forall h t. Proxy h -> Proxy t -> Proxy (Cons h t)
proxyCons _ _ = Proxy

-- =============================================================================
-- GroupMethodBuilder

data SomeGroupBuilder
  = Column
  | Row

derive instance Generic SomeGroupBuilder _

instance EncodeJson SomeGroupBuilder where
  encodeJson x = genericEncodeJson x

instance DecodeJson SomeGroupBuilder where
  decodeJson x = genericDecodeJson x

fromSomeGroupBuilder :: SomeGroupBuilder -> Result SomeGroup
fromSomeGroupBuilder = case _ of
  Column -> pure (mkSomeGroup (Proxy :: Proxy Column))
  Row -> pure (mkSomeGroup (Proxy :: Proxy Row))

-- =============================================================================
-- SomeStyleBuilder

data SomeStyleBuilder = Quote

derive instance Generic SomeStyleBuilder _

instance EncodeJson SomeStyleBuilder where
  encodeJson x = genericEncodeJson x

instance DecodeJson SomeStyleBuilder where
  decodeJson x = genericDecodeJson x

fromSomeStyleBuilder :: SomeStyleBuilder -> Result SomeStyle
fromSomeStyleBuilder = case _ of
  Quote -> pure (mkSomeStyle (Proxy :: Proxy Quote))

-- =============================================================================
-- Named Content

-- This is the function the looks up content by name. These should correspond to
-- the actual names of the singleton types that define the content.
fromNamedSomeContentBuilder :: String -> Result SomeContent
fromNamedSomeContentBuilder name = case Map.lookup name namedSomeContent of
  Nothing -> throwError ("unknown name: " <> show name)
  Just some_content -> pure some_content

namedSomeContent :: Map String SomeContent
namedSomeContent = Map.fromFoldable
  [ "NoteA" /\ mkSomeContent (Proxy :: Proxy NoteA)
  , "NoteB" /\ mkSomeContent (Proxy :: Proxy NoteB)
  ]


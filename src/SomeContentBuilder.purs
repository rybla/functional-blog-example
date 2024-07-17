module SomeContentBuilder where

import Content
import Data.Generic.Rep
import Prelude
import Prim hiding (Row)

import Content.Notes (namedSomeContent)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Eq.Generic (genericEq)
import Data.Foldable (fold)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Halogen.HTML as HH
import HalogenUtils as HU
import Parsing (ParseError(..), Parser, position, runParser)
import Parsing.Combinators (manyTill, (<|>))
import Parsing.String (anyCodePoint, char, string)
import Partial.Unsafe (unsafeCrashWith)
import Type.Prelude (class IsSymbol, Proxy(..), reifySymbol)

-- =============================================================================

type Result = Either String

-- =============================================================================
-- SomeContentBuilder

data SomeContentBuilder
  = Literal String
  | Named String
  | Styled SomeStyleBuilder SomeContentBuilder
  | Grouped SomeGroupBuilder SomeContentListBuilder
  | Hole

derive instance Generic SomeContentBuilder _

instance EncodeJson SomeContentBuilder where
  encodeJson x = genericEncodeJson x

instance DecodeJson SomeContentBuilder where
  decodeJson x = genericDecodeJson x

renderSomeContentBuilder :: SomeContentBuilder -> Array ContentHTML
renderSomeContentBuilder = fromSomeContentBuilder >>> case _ of
  Left err ->
    [ HH.div
        [ HU.style [ "padding: 0.5em", "background-color: red" ] ]
        [ HH.text ("[error] invalid SomeContentBuilder: " <> err) ]
    ]
  Right some_content -> some_content # renderFinalSomeContent

fromSomeContentBuilder :: SomeContentBuilder -> Result SomeContent
fromSomeContentBuilder = case _ of
  Literal str -> reifySymbol str \sym -> pure (mkSomeContent (proxyLiteral sym))
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
  Hole -> pure (mkSomeContent (Proxy :: Proxy Hole))

proxyLiteral :: forall sym. Proxy sym -> Proxy (Literal sym)
proxyLiteral _ = Proxy

proxyGrouped :: forall group list. Proxy group -> Proxy list -> Proxy (Grouped group list)
proxyGrouped _ _ = Proxy

proxyStyled :: forall style content. Proxy style -> Proxy content -> Proxy (Styled style content)
proxyStyled _ _ = Proxy

-- =============================================================================
-- SomeContentListBuilder

data SomeContentListBuilder
  = Nil
  | Cons SomeContentBuilder SomeContentListBuilder

infixr 6 Cons as :

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

instance Show SomeGroupBuilder where
  show x = genericShow x

instance Eq SomeGroupBuilder where
  eq x = genericEq x

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

data SomeStyleBuilder
  = Title
  | Section
  | Subsection
  | Block
  | Quote
  | Code

derive instance Generic SomeStyleBuilder _

instance Show SomeStyleBuilder where
  show x = genericShow x

instance Eq SomeStyleBuilder where
  eq x = genericEq x

instance EncodeJson SomeStyleBuilder where
  encodeJson x = genericEncodeJson x

instance DecodeJson SomeStyleBuilder where
  decodeJson x = genericDecodeJson x

fromSomeStyleBuilder :: SomeStyleBuilder -> Result SomeStyle
fromSomeStyleBuilder = case _ of
  Title -> pure (mkSomeStyle (Proxy :: Proxy Title))
  Section -> pure (mkSomeStyle (Proxy :: Proxy Section))
  Subsection -> pure (mkSomeStyle (Proxy :: Proxy Subsection))
  Block -> pure (mkSomeStyle (Proxy :: Proxy Block))
  Quote -> pure (mkSomeStyle (Proxy :: Proxy Quote))
  Code -> pure (mkSomeStyle (Proxy :: Proxy Code))

-- =============================================================================
-- Named Content

-- This is the function the looks up content by name. These should correspond to
-- the actual names of the singleton types that define the content.
fromNamedSomeContentBuilder :: String -> Result SomeContent
fromNamedSomeContentBuilder name = case Map.lookup name namedSomeContent of
  Nothing -> throwError ("unknown name: " <> show name)
  Just some_content -> pure some_content

-- =============================================================================
-- EncodeMu and DecodeMu (micro encoding format)

class EncodeMu a where
  encodeMu :: a -> String

class DecodeMu a where
  parseMu :: Unit -> Parser String a

decodeMu :: forall a. DecodeMu a => String -> ParseError \/ a
decodeMu s = runParser s (parseMu unit)

-- main instances

instance EncodeMu SomeContentBuilder where
  encodeMu x = generic_encodeMu x

instance DecodeMu SomeContentBuilder where
  parseMu x = generic_parseMu x

instance EncodeMu SomeContentListBuilder where
  encodeMu x = generic_encodeMu x

instance DecodeMu SomeContentListBuilder where
  parseMu x = generic_parseMu x

instance EncodeMu SomeStyleBuilder where
  encodeMu x = generic_encodeMu x

instance DecodeMu SomeStyleBuilder where
  parseMu x = generic_parseMu x

instance EncodeMu SomeGroupBuilder where
  encodeMu x = generic_encodeMu x

instance DecodeMu SomeGroupBuilder where
  parseMu x = generic_parseMu x

-- Generic_EncodeMu and Generic_DecodeMu

class Generic_EncodeMu a where
  generic_encodeMu' :: a -> String

class Generic_EncodeMuArgs a where
  generic_encodeMuArgs :: a -> String

class Generic_DecodeMu a where
  generic_parseMu' :: Unit -> Parser String a

class Generic_DecodeMuArgs a where
  generic_parseMuArgs :: Unit -> Parser String a

instance Generic_EncodeMu NoConstructors where
  generic_encodeMu' x = generic_encodeMu' x

instance Generic_DecodeMu NoConstructors where
  generic_parseMu' x = generic_parseMu' x

instance Generic_EncodeMuArgs NoArguments where
  generic_encodeMuArgs _ = ""

instance Generic_DecodeMuArgs NoArguments where
  generic_parseMuArgs _ = pure NoArguments

instance (Generic_EncodeMu a, Generic_EncodeMu b) => Generic_EncodeMu (Sum a b) where
  generic_encodeMu' (Inl a) = "0" <> generic_encodeMu' a
  generic_encodeMu' (Inr b) = "1" <> generic_encodeMu' b

instance (Generic_DecodeMu a, Generic_DecodeMu b) => Generic_DecodeMu (Sum a b) where
  generic_parseMu' _ = do
    (string "0" <|> string "1") >>= case _ of
      "0" -> Inl <$> generic_parseMu' unit
      "1" -> Inr <$> generic_parseMu' unit
      _ -> unsafeCrashWith "impossible"

instance (Generic_EncodeMuArgs a, Generic_EncodeMuArgs b) => Generic_EncodeMuArgs (Product a b) where
  generic_encodeMuArgs (Product a b) = "0" <> generic_encodeMuArgs a <> "1" <> generic_encodeMuArgs b

instance (Generic_DecodeMuArgs a, Generic_DecodeMuArgs b) => Generic_DecodeMuArgs (Product a b) where
  generic_parseMuArgs _ = do
    _ <- string "0"
    a <- generic_parseMuArgs unit
    _ <- string "1"
    b <- generic_parseMuArgs unit
    pure (Product a b)

instance Generic_EncodeMuArgs a => Generic_EncodeMu (Constructor name a) where
  generic_encodeMu' (Constructor a) = generic_encodeMuArgs a

instance Generic_DecodeMuArgs a => Generic_DecodeMu (Constructor name a) where
  generic_parseMu' _ = Constructor <$> generic_parseMuArgs unit

instance EncodeMu a => Generic_EncodeMuArgs (Argument a) where
  generic_encodeMuArgs (Argument a) = encodeMu a

instance DecodeMu a => Generic_DecodeMuArgs (Argument a) where
  generic_parseMuArgs _ = Argument <$> parseMu unit

generic_encodeMu :: forall a rep. Generic a rep => Generic_EncodeMu rep => a -> String
generic_encodeMu x = generic_encodeMu' (from x)

generic_parseMu :: forall a rep. Generic a rep => Generic_DecodeMu rep => Unit -> Parser String a
generic_parseMu x = generic_parseMu' x <#> to

-- utility instances

instance EncodeMu String where
  encodeMu s = s <> ".String"

instance DecodeMu String where
  parseMu _ = do
    cps <- anyCodePoint `manyTill` (string ".String")
    pure (cps # Array.fromFoldable # String.fromCodePointArray)

instance EncodeMu Int where
  encodeMu i = show i <> ".Int"

instance DecodeMu Int where
  parseMu _ = do
    cps <- anyCodePoint `manyTill` (string ".Int")
    let s = cps # Array.fromFoldable # String.fromCodePointArray
    pos <- position
    s # Int.fromString # maybe (throwError (ParseError ("expected an Int, but found " <> show s) pos)) pure

instance EncodeMu Unit where
  encodeMu _ = ".Unit"

instance DecodeMu Unit where
  parseMu _ = do
    _ <- string ".Unit"
    pure unit
module GenerateNotes where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Foldable (foldMap, intercalate, traverse_)
import Data.List ((:))
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.String as String
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (error)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS

main :: Effect Unit
main = do
  let notes_dir = "./src/Content/Notes/"
  -- collect all teh note name  that appear in the files
  notes_ref <- Ref.new mempty
  notes_files <- FS.readdir notes_dir
  notes_files # traverse_ \note_file -> do
    moduleName <- note_file
      # String.stripSuffix (String.Pattern ".purs")
      # maybe (throwError (error ("there is a file in directory " <> show notes_dir <> " that doesn't end with " <> show ".purs" <> "."))) pure
    note_content <- FS.readTextFile UTF8 (notes_dir <> note_file)
    let note_lines = note_content # String.split (String.Pattern "\n")
    note_lines # traverse_ \note_line -> do
      case note_line # String.stripPrefix (String.Pattern "--Content ") of
        Nothing -> pure unit
        Just noteName -> notes_ref # Ref.modify_ ({ moduleName, noteName, noteTypeName: "Content_" <> noteName } : _)
  notes <- Ref.read notes_ref
  -- create the Content.Notes module
  -- let imports = notes # foldMap (\{ moduleName } -> "import Content.Notes." <> moduleName <> " as " <> moduleName <> "\n")
  let imports = notes # map _.moduleName # Set.fromFoldable # foldMap (\moduleName -> "import Content.Notes." <> moduleName <> " as " <> moduleName <> "\n")
  let instances = notes # foldMap (\{ moduleName, noteName, noteTypeName } -> 
        """
foreign import data """ <> noteTypeName <> """ :: ContentKind
instance Content """ <> noteTypeName <> """ where
  renderContent _ = """ <> moduleName <> "." <> noteName <> """

""")

  let mappings = 
        notes 
          # map (\{ noteName, noteTypeName } -> "\"" <> noteName <> "\" /\\ mkSomeContent (Proxy :: Proxy " <> noteTypeName <> ")")
          # intercalate "\n  , "

  let notesModule_content = 
        ("""
module Content.Notes where

import Content (class Content, ContentKind, SomeContent, mkSomeContent)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Type.Prelude (Proxy(..))
""" <> imports <> """

""" <> instances <> """

namedSomeContent :: Map String SomeContent
namedSomeContent = Map.fromFoldable
  [ """ <> mappings <> """
  ]
        """ # String.trim)
  
  let notesModule_file = "./src/Content/Notes.purs"
  FS.writeTextFile UTF8 notesModule_file notesModule_content
  Console.log ("wrote module " <> notesModule_file)
  pure unit
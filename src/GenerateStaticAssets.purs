module GenerateStaticAssets where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Data.Argonaut (decodeJson, parseJson)
import Data.Array as Array
import Data.Either (either)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe', maybe, maybe')
import Data.String as String
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception (error)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS

-- for each Purescript file in `/src/Content/Pages`
-- - generate each entrypoint (Javascript file that imports a runnable compiled
--   Purescript module and runs it), which will each go into `/entrypoints`
--   - the name of the generated entrypoint should be of the form `<path>.js`
--     where `path` will be the name of the directory in `/docs` that will
--     contain the bundling of this entrypoint as a `main.js`
-- - generate each route directory and child `index.html`, which will each go into
--   `/docs`

type Props =
  { public_path :: String
  }

main :: Effect Unit
main = do
  let pages_dir = "src/Content/Pages/"
  page_files <- FS.readdir pages_dir
  builder_files_ref <- Ref.new ([] :: Array String)
  page_files # traverse_ \page_file -> do

    content <- FS.readTextFile UTF8 (pages_dir <> page_file)
    let lines = content # String.split (String.Pattern "\n")
    firstline <- lines # Array.head # maybe (throwError (error ("file \"" <> page_file <> "\" is empty"))) pure
    props_string <- firstline # String.stripPrefix (String.Pattern "--props ") # maybe (throwError (error ("first line must begin with \"--props \""))) pure
    props_json <- props_string # parseJson # either (\err -> throwError (error (show err))) pure
    props :: Props <- decodeJson props_json # either (\err -> throwError (error (show err))) pure

    -- generate entrypoint
    let entrypoint_file = "entrypoints/" <> props.public_path <> ".js"
    FS.writeTextFile UTF8 entrypoint_file
      ( """
import { start_client } from '../output/Content.Pages.""" <> (page_file # String.replace (String.Pattern ".purs") (String.Replacement "/index.js")) <>
          """'
start_client()
        """ # String.trim
      )
    Console.log ("wrote entrypoint   \"" <> entrypoint_file <> "\"")

    -- generate script to generate public assets
    --   generate Javascript that will generate the static assets
    --   this needs to be a generated script because I can't dynamically import files in Purescript (which makes sense of course)
    let
      public_dir =
        if props.public_path == "index" then
          "docs/"
        else
          "docs/" <> props.public_path <> "/"
    flip catchError (const (pure unit)) do
      FS.mkdir public_dir
      Console.log ("made  directory   \"" <> public_dir <> "\"")
    let public_index = public_dir <> "index.html"
    let public_main = public_dir <> "main.js"
    let generator_file = "generators/" <> props.public_path <> ".js"
    FS.writeTextFile UTF8 generator_file
      ( """
import { static_content } from '../output/Content.Pages.""" <> (page_file # String.replace (String.Pattern ".purs") (String.Replacement "/index.js"))
          <>
            """'
import * as fs from 'node:fs';
const content = static_content;
fs.writeFile('"""
          <> public_index
          <>
            """', content, err => { if (err) { console.error(err) } });
        """ # String.trim
      )
    Console.log ("wrote generator    \"" <> generator_file <> "\"")

    -- builder
    let builder_file = "builders/" <> props.public_path <> ".js"
    builder_files_ref # Ref.modify_ (Array.cons builder_file)
    FS.writeTextFile UTF8 builder_file
      ( """
import { execSync } from 'node:child_process'
execSync('pnpm node """ <> generator_file
          <>
            """');
execSync('pnpm esbuild """
          <> entrypoint_file
          <> """ --bundle --outfile="""
          <> public_main
          <>
            """');
        """ # String.trim
      )
    Console.log ("wrote builder      \"" <> builder_file <> "\"")
    pure unit

  -- superbuilder
  builder_files <- builder_files_ref # Ref.read
  let superbuilder_file = "superbuilder.js"
  FS.writeTextFile UTF8 superbuilder_file
    ( """
import { execSync } from 'node:child_process'
"""
        <> (builder_files # Array.foldMap (\builder_file -> "execSync('pnpm node " <> builder_file <> "');\n"))
        <>
          """
      """ # String.trim
    )
  Console.log ("wrote superbuilder \"" <> superbuilder_file <> "\"")
  pure unit

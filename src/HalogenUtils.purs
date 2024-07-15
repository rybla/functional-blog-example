module HalogenUtils where

import Prelude

import Data.Array as Array
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

style :: forall props a. Array String -> HH.IProp (style :: String | props) a
style = HP.style <<< Array.intercalate "; "

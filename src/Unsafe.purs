module Unsafe where

import Prelude

import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)

fromJust :: forall a. Maybe a -> a
fromJust = case _ of
  Nothing -> unsafeCrashWith "fromJust Nothing"
  Just a -> a


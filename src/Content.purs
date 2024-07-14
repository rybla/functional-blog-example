module Content where

import Prelude

import Content.Pages.Example1 as Pages.Example1
import Page as Page

page_specs :: Array Page.Spec
page_specs =
  [ Pages.Example1.spec
  ]
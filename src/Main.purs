module Main where

import Prelude

import Effect (Effect)
-- import Effect.Console (log)

-- import Halogen.HTML as H
-- import Halogen.HTML.Properties as HP

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import FileSystem as FS


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  -- log "Hello, World!"
  _ <- runUI FS.component unit body
  pure unit


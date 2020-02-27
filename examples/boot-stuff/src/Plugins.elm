module Plugins exposing (plugins)

import Elmercy.System exposing (Plugin)
import Elmercy.Plugin.OnePage
import Elmercy.Plugin.SPA


plugins : List Plugin
plugins =
  [ Elmercy.Plugin.OnePage.plugin
  , Elmercy.Plugin.SPA.plugin
  ]
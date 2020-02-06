module App.Main exposing (main)

import Browser
import Main


main : Program () Main.Model Main.Msg
main =
    Browser.sandbox
        { init = Main.init
        , view = Main.view
        , update = Main.update
        }

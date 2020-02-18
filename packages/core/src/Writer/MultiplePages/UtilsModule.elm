module Writer.MultiplePages.UtilsModule exposing (write)


write : String
write =
    """module App.Utils exposing (mapDocument, dispatch)

import Browser exposing (Document)
import Html
import Task



-- BASIC


dispatch : msg -> Cmd msg
dispatch =
    Task.succeed >> Task.perform identity



-- HTML


mapDocument : (msg1 -> msg2) -> Document msg1 -> Document msg2
mapDocument f { title, body } =
    { title = title
    , body =
        List.map (Html.map f) body
    }
  """

module FileSystem where

import Prelude

import Halogen as H
import Halogen.HTML as H
import Halogen.HTML.Events as HE

-------------Input-----------------
type Input = Unit

-------------Model-----------------
type Model =
  { count :: Int
  }

initModel :: Input -> Model
initModel _ =
  { count: 0
  }

-------------Msg-----------------
data Msg =
    Increment
  | Decrement

-------------Update-----------------
update :: forall output m . Msg -> H.HalogenM Model Msg () output m Unit
update =
  case _ of
    Increment ->
      H.modify_ \model -> model { count = model.count + 1 }

    Decrement ->
      H.modify_ \model -> model { count = model.count - 1 }

-------------VIEWS-----------------

view :: forall m . Model -> H.ComponentHTML Msg () m
view model =
  H.div []
    [ H.text $ show model.count
    , H.button [ HE.onClick \_ -> Increment ] [ H.text "inc" ]
    , H.button [ HE.onClick \_ -> Decrement ] [ H.text "dec" ]
    ]

-------------Component-----------------
component :: forall query output m . H.Component query Input output m
component =
  H.mkComponent
    { initialState: initModel
    , render: view
    , eval: H.mkEval H.defaultEval { handleAction = update }
    }

module HomePage exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)

type alias Model = {
    username: String,
    password: String,
    spending: List 
}

initialModel: Model 
initialModel = {
    username="",
    password="",
    spending=[]
}

type Msg
    = Name String 
    | Password String

update : Msg -> Model -> Model  
update msg model = 
    case msg of 
    Login -> 

view : Model -> Html Msg
view model =
    div [ class "jumbotron" ]
        [ h1 [] [ text "Expense Tracker" ]
        , p []
            [ text "Welcome to our expense tracker application!! "]
        , viewPiechart model.spending
        ]

viewPiechart : () -> Html Msg
viewPiechart _ =  
    text [ "graphic here" ]

main : Program () Model Msg
main =
  Browser.sandbox { init = init, 
                    update = update,
                    view = view }
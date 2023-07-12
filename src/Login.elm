module Login exposing (..)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, type_)
import Html.Events exposing (onClick, onInput)

type alias Model =
    { username : String
    , password : String
    , isLoggedIn : Bool
    }

init : Model
init =
    { username = ""
    , password = ""
    , isLoggedIn = False
    }

type Msg
    = UpdateUsername String
    | UpdatePassword String
    | Login
    | Logout

update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateUsername username ->
            { model | username = username }

        UpdatePassword password ->
            { model | password = password }

        Login ->
            -- Implement your login logic here
            if model.username == "admin" && model.password == "admin" then
                { model | isLoggedIn = True }
            else
                model --dann unverändert
        Logout ->
            { model | isLoggedIn = False }

view : Model -> Html Msg
view model =
    if model.isLoggedIn then
        div []
            [ text ("Logged in as: " ++ model.username)
            , button [ onClick Logout ] [ text "Logout" ]
            ]
    else
        div []
            [ input [ type_ "text", placeholder "Username", onInput UpdateUsername ] []
            , input [ type_ "password", placeholder "Password", onInput UpdatePassword ] []
            , button [ onClick Login ] [ text "Login" ]
            ]


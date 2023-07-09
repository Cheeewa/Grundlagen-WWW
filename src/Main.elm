module Main exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
type alias Expense =
    { description : String
    , amount : Float
    }
type alias Model =
    { expenses : List Expense
    , newExpenseDescription : String
    , newExpenseAmount : String
    }
type Msg
    = AddExpense
    | NewExpenseDescription String
    | NewExpenseAmount String
init : Model
init =
    { expenses = []
    , newExpenseDescription = ""
    , newExpenseAmount = ""
    }
update : Msg -> Model -> Model
update msg model =
    case msg of
    AddExpense -> 
        { model | expenses =
           { description = model.newExpenseDescription, amount = Maybe.withDefault 0 (String.toFloat model.newExpenseAmount)
           }
           :: model.expenses
           , newExpenseDescription = ""
           , newExpenseAmount = ""
        }
    NewExpenseDescription description ->
        { model | newExpenseDescription = description }
    NewExpenseAmount amount ->
        { model | newExpenseAmount = amount }
view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Budget Tracker" ]
        , Html.form [ onSubmit AddExpense ]
            [ div []
                [ viewInput "Description" model.newExpenseDescription NewExpenseDescription
                , viewInput "Amount" model.newExpenseAmount NewExpenseAmount
                , button [] [ text "Add Expense" ]
                ]
            ]
        , ul [] (List.map expenseView model.expenses)
    ]

viewInput : String -> String -> (String -> Msg) -> Html Msg
viewInput p v toMsg = 
    input [ placeholder p, value v, onInput toMsg ] []

expenseView : Expense -> Html msg
expenseView expense =
    li []
        [ text (expense.description ++ ": " ++ String.fromFloat expense.amount)
        ]
main : Program () Model Msg
main =
    Browser.sandbox
    { init = init
    , view = view
    , update = update
    }

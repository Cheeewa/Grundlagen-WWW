module Main exposing (..)
{-- --}
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import PieChart exposing (main)

type alias Expense =
    { description : String
    , amount : Int
    }
type alias Model =
    { expenses : List Expense
    , budget : Int
    , newExpenseDescription : String
    , newExpenseAmount : String
    , newBudget : String
    }
type Msg
    = AddExpense
    | AddBudget
    | NewExpenseDescription String
    | NewExpenseAmount String
    | NewBudget String
init : Model
init =
    { expenses = []
    , newExpenseDescription = ""
    , newExpenseAmount = ""
    , budget = 0
    , newBudget = ""
    }
update : Msg -> Model -> Model
update msg model =
    case msg of
    AddExpense -> 
        { model | expenses =
           { description = model.newExpenseDescription, amount = Maybe.withDefault 0 (String.toInt model.newExpenseAmount) 
           }
           :: model.expenses
           , newExpenseDescription = ""
           , newExpenseAmount = ""
        }
    AddBudget -> 
        { model | budget =
            model.budget + Maybe.withDefault 0 (String.toInt model.newBudget)
            , newBudget = ""
        }
    NewExpenseDescription description ->
        { model | newExpenseDescription = description }
    NewExpenseAmount amount ->
        { model | newExpenseAmount = amount }
    NewBudget amount ->
        {model | newBudget = amount}
view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Budget Tracker" ]
        ,  text ( "Budget: " ++ String.fromInt model.budget)
        , Html.form [ onSubmit AddBudget ]
            [ div []
                [ viewInput "Budget" model.newBudget NewBudget
                , button [] [ text "Add Budget" ]
                ]
            ]
        , Html.form [ onSubmit AddExpense ]
            [ div []
                [ viewInput "Description" model.newExpenseDescription NewExpenseDescription
                , viewInput "Amount" model.newExpenseAmount NewExpenseAmount
                , button [] [ text "Add Expense" ]
                ]
            ]
        , ul [] (List.map expenseView model.expenses)
        ,  
            PieChart.main 
    ]
    

viewInput : String -> String -> (String -> Msg) -> Html Msg
viewInput p v toMsg = 
    input [ placeholder p, value v, onInput toMsg ] []

expenseView : Expense -> Html msg
expenseView expense =
    li []
        [ text ( descriptionFill expense.description ++ ": " ++ String.fromInt expense.amount)
        ]

descriptionFill : String -> String
descriptionFill description =
    case description of
        "" ->
            "unknown"
        _ -> 
            description



main : Program () Model Msg
main =
    Browser.sandbox
    { init = init
    , view = view
    , update = update
    }
    


module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import PieChart


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
    , sum : Int
    }


type Msg
    = AddExpense
    | AddBudget
    | NewExpenseDescription String
    | NewExpenseAmount String
    | NewBudget String
    | AddValue


init : Model
init =
    { expenses = []
    , newExpenseDescription = ""
    , newExpenseAmount = ""
    , budget = 0
    , newBudget = ""
    , sum = 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddExpense ->
            { model | expenses =
                { description = model.newExpenseDescription, amount = Maybe.withDefault 0 (String.toInt model.newExpenseAmount) }
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
            { model | newBudget = amount }
        AddValue ->
                { model | sum = model.sum + model.budget }


view : Model -> Html Msg
view model =
    div [style "background-color" "#F9E2AF" ]
        [h1 [ style "color" "black"
             ,style "text-align" "center"]
             [ img [ src "assets/icon1.png"
                , style "max-height" "20px"
                     ] []
             ,text "BUDGET TRACKER" ]
          ,p [ style "text-align" "center"
             , style "font-size" "24px"
             , style "font-weight" "bold"
             , style "color" "black"]
             [text ( "BUDGET : " ++ String.fromInt model.budget ++" €")
             ]
         , Html.form [ onSubmit AddBudget ]
            [ div [ style "text-align" "center"]
                  [ viewInput "Budget" model.newBudget NewBudget
                    ,  button [style "color" "#FFF3E2"
                              ,style "background-color" "#F97B22"
                              , style "font-weight" "bold"]
                    [ text "Add Budget" ]
                ]
            ]
         , br[] []
         , Html.form [ onSubmit AddExpense ]
            [ div [style "text-align" "center"
                    ]
                [ viewInput "Description" model.newExpenseDescription NewExpenseDescription
                , text " "
                , viewInput "Amount" model.newExpenseAmount NewExpenseAmount
                , button [style "color" "#FFF3E2"
                         ,style "background-color" "#F97B22"
                         ,style "font-weight" "bold"]
                         [ text "Add Expense" ]
                ]
            ]
            , ul [style "color" "grey"
                ,style "text-align" "center"
                ,style "background-color" "lightgrey"
                ,style "font-size" "22px"
                ,style "padding" "0.5"
                ]
                (List.map expenseView model.expenses)
            , PieChart.main
            ]
        
viewInput : String -> String -> (String -> Msg) -> Html Msg
viewInput p v toMsg =
    input [ placeholder p, value v, onInput toMsg ] []


expenseView : Expense -> Html msg
expenseView expense =
    li []
        [ text ( descriptionFill expense.description ++ ": " ++ String.fromInt expense.amount )
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

module Main exposing (..)
{-- --}
import Browser exposing (UrlRequest, Document)
import Browser.Navigation as Nav exposing (Key)
import Html exposing (Html, div, h1, text, ul, input, li, button, a, b, br, p, img)
import Url exposing (Url)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import PieChart exposing ( update)
import Login
import List.Extra exposing (last)
import TypedSvg.Core exposing (Svg, text)


-- MAIN

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



type alias Expense =
    { description : String
    , amount : Int
    }


type alias Model =
    { expenses : List Expense
    , budget : Int
    , spent : Int
    , newExpenseDescription : String
    , newExpenseAmount : String
    , newBudget : String
    , key : Nav.Key
    , url : Url.Url
    }


type Msg
    = AddExpense
    | AddBudget
    | NewExpenseDescription String
    | NewExpenseAmount String
    | NewBudget String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url

    
init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init flag url key =
    ( Model [] 0 0 "" "" "" key url, Cmd.none )

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of

    AddExpense -> 
        let
            newamount = Maybe.withDefault 0 (String.toInt model.newExpenseAmount)
            oldSpent = model.spent 
        in
        
            ({ model | expenses =
            { description = model.newExpenseDescription, amount = newamount
             }
            :: model.expenses
            , newExpenseDescription = ""
            , newExpenseAmount = ""
            , spent = oldSpent - newamount
            }, Cmd.none)

    AddBudget -> 
        let 
            newbudget = Maybe.withDefault 0 (String.toInt model.newBudget)
         in
            ({ model | budget =
            model.budget + newbudget
            , newBudget = ""
            }, Cmd.none)

    NewExpenseDescription description ->
        ({ model | newExpenseDescription = description }, Cmd.none)

    NewExpenseAmount amount ->
        ({ model | newExpenseAmount = amount }, Cmd.none)

    NewBudget amount ->
        ({model | newBudget = amount}, Cmd.none)

    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )

view : Model -> Browser.Document Msg
view model =
    { title = "Budget Tracking App"
    , body = 
        [ 
         b [] [  ] --Html.text (Url.toString model.url)
        , ul []
            [ viewLink "/home"
            , viewLink "/history"
            , div [style "background-color" "#F9E2AF" ]
                [h1 [style "color" "black"
                  ,style "text-align" "center"]
                  [ img [ src "assets/icon1.png"
                  , style "max-height" "20px"
                      ] []
                  ,Html.text "BUDGET TRACKER" ]
                  ,p [ style "text-align" "center"
                     , style "font-size" "24px"
                     , style "font-weight" "bold"
                     , style "color" "black"]
                     [Html.text ( "BUDGET : " ++ String.fromInt model.budget ++" € BALANCE : " ++ String.fromInt (model.budget + model.spent) ++ "€")
                     ]
                 , Html.form [ onSubmit AddBudget ]
                    [ div [ style "text-align" "center"]
                          [ viewInput "Budget" model.newBudget NewBudget
                            ,  button [style "color" "#FFF3E2"
                                      ,style "background-color" "#F97B22"
                                      , style "font-weight" "bold"]
                            [ Html.text "Add Budget" ]
                        ]
                    ]
                 , br[] []
                 , Html.form [ onSubmit AddExpense ]
                    [ div [style "text-align" "center"
                            ]
                        [ viewInput "Description" model.newExpenseDescription NewExpenseDescription
                        , Html.text " "
                        , viewInput "Amount" model.newExpenseAmount NewExpenseAmount
                        , button [style "color" "#FFF3E2"
                                 ,style "background-color" "#F97B22"
                                 ,style "font-weight" "bold"]
                                 [ Html.text "Add Expense" ]
                        ]
                    ]
                    , ul [style "color" "grey"
                        ,style "text-align" "center"
                        ,style "background-color" "lightgrey"
                        ,style "font-size" "22px"
                        ,style "padding" "0.5"
                        ]
                        (List.map expenseView model.expenses)
                    , viewPieChart model.budget model.spent 0
                ]
            ]
        ]
    }


viewPieChart : Int -> Int -> Int -> Svg msg
viewPieChart budget spent minus = 
    let balancedata = toFloat (budget + spent)
        spentdata = toFloat -spent
        
    in
    PieChart.update [ ( showLabel balancedata " Balance: " ++ String.fromFloat balancedata, balancedata )
                    , ( showLabel spentdata " Spent:" ++ String.fromFloat spentdata , spentdata )
                    , ( "", toFloat minus )
                    ]

showLabel : Float -> String -> String
showLabel balance text =
    if balance == 0 then ""
    else text

viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ Html.text path ] ]

viewInput : String -> String -> (String -> Msg) -> Html Msg
viewInput p v toMsg =
    input [ placeholder p, value v, onInput toMsg ] []


expenseView : Expense -> Html msg
expenseView expense =
    li []
        [ Html.text ( descriptionFill expense.description ++ ": " ++ String.fromInt expense.amount )
        ]


descriptionFill : String -> String
descriptionFill description =
    case description of
        "" ->
            "unknown"
        _ ->
            description


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


    


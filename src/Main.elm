module Main exposing (..)

import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Process exposing (sleep)
import Random
import Task
import Tuple exposing (first, second)


type alias Flags =
    { randSeed : Int }


type alias Model =
    { deck : List Card
    , cardsToCompare : ( Card, Card )
    , canClickCards : Bool
    , score : Int
    , gameEnd : Bool
    , seed : Random.Seed
    }


type alias Card =
    { id : Int
    , value : String
    , faceDown : Bool
    , comparing : Bool
    }


blankCard : Card
blankCard =
    { id = 0, value = "", faceDown = True, comparing = False }


init : Flags -> ( Model, Cmd Msg )
init { randSeed } =
    ( { deck =
            [ { id = 1, value = "A", faceDown = True, comparing = False }
            , { id = 2, value = "A", faceDown = True, comparing = False }
            , { id = 3, value = "B", faceDown = True, comparing = False }
            , { id = 4, value = "B", faceDown = True, comparing = False }
            , { id = 5, value = "C", faceDown = True, comparing = False }
            , { id = 6, value = "C", faceDown = True, comparing = False }
            , { id = 7, value = "D", faceDown = True, comparing = False }
            , { id = 8, value = "D", faceDown = True, comparing = False }
            , { id = 9, value = "E", faceDown = True, comparing = False }
            , { id = 10, value = "E", faceDown = True, comparing = False }
            , { id = 11, value = "F", faceDown = True, comparing = False }
            , { id = 12, value = "F", faceDown = True, comparing = False }
            , { id = 13, value = "G", faceDown = True, comparing = False }
            , { id = 14, value = "G", faceDown = True, comparing = False }
            , { id = 15, value = "H", faceDown = True, comparing = False }
            , { id = 16, value = "H", faceDown = True, comparing = False }
            , { id = 17, value = "I", faceDown = True, comparing = False }
            , { id = 18, value = "I", faceDown = True, comparing = False }
            , { id = 19, value = "J", faceDown = True, comparing = False }
            , { id = 20, value = "J", faceDown = True, comparing = False }
            , { id = 21, value = "K", faceDown = True, comparing = False }
            , { id = 22, value = "K", faceDown = True, comparing = False }
            , { id = 23, value = "L", faceDown = True, comparing = False }
            , { id = 24, value = "L", faceDown = True, comparing = False }
            , { id = 25, value = "M", faceDown = True, comparing = False }
            , { id = 26, value = "M", faceDown = True, comparing = False }
            , { id = 27, value = "N", faceDown = True, comparing = False }
            , { id = 28, value = "N", faceDown = True, comparing = False }
            , { id = 29, value = "O", faceDown = True, comparing = False }
            , { id = 30, value = "O", faceDown = True, comparing = False }
            , { id = 31, value = "P", faceDown = True, comparing = False }
            , { id = 32, value = "P", faceDown = True, comparing = False }
            , { id = 33, value = "Q", faceDown = True, comparing = False }
            , { id = 34, value = "Q", faceDown = True, comparing = False }
            , { id = 35, value = "R", faceDown = True, comparing = False }
            , { id = 36, value = "R", faceDown = True, comparing = False }
            ]
      , cardsToCompare = ( blankCard, blankCard )
      , canClickCards = True
      , score = 0
      , gameEnd = False
      , seed = Random.initialSeed randSeed
      }
    , newGame
    )


type Msg
    = NoOp
    | NewGame
    | FlipSingleCard Card
    | RestoreComparing
    | RestoreComparingAndFlipped


view : Model -> Html Msg
view model =
    let
        viewCard card =
            div
                [ onClick <|
                    (if card.faceDown && model.canClickCards then
                        FlipSingleCard
                     else
                        always NoOp
                    )
                    <|
                        card
                , classList [ ( "card", True ), ( "face-down", card.faceDown ) ]
                ]
                [ Html.span [ class "card-value" ] [ text <| card.value ]
                ]

        gameEndView =
            div [ class "game-end" ]
                [ h1 [] [ text "Game Over" ]
                , p [] [ text <| "Great job! You made " ++ (toString model.score) ++ " mismatches. Try for fewer next time!" ]
                ]
    in
        div []
            [ header []
                [ div [ class "header-item" ]
                    [ span [ class "logo-first-half" ] [ text "elm" ]
                    , span [ class "logo-second-half" ] [ text "emory" ]
                    ]
                , newGameButton
                , scoreDisplay model
                ]
            , if model.gameEnd then
                gameEndView
              else
                div [ class "board" ]
                    [ div [] <|
                        List.map
                            viewCard
                        <|
                            model.deck
                    ]
            ]


scoreDisplay : Model -> Html Msg
scoreDisplay model =
    div [ class "header-item" ] [ span [ class "score" ] [ text <| toString model.score ] ]


newGameButton : Html Msg
newGameButton =
    div [ classList [ ( "header-item", True ), ( "new-game", True ) ] ] [ button [ onClick NewGame ] [ text "New Game" ] ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewGame ->
            let
                newModel =
                    model
                        |> resetGameEnd
                        |> resetScore
                        |> flipAllCardsFaceDown
                        |> shuffle
            in
                ( newModel, Cmd.none )

        FlipSingleCard card ->
            let
                flipCard e =
                    if e.id == card.id then
                        { e | faceDown = False, comparing = True }
                    else
                        e

                updatedDeck =
                    List.map flipCard model.deck

                updatedCardsToCompare =
                    (,) card (first model.cardsToCompare)

                numberComparing =
                    let
                        i =
                            0
                    in
                        List.length <|
                            List.filter (\val -> val == 1) <|
                                List.map
                                    (\card ->
                                        if card.comparing == True then
                                            i + 1
                                        else
                                            i
                                    )
                                    updatedDeck

                areEqual =
                    if numberComparing == 2 then
                        (==) (.value <| first updatedCardsToCompare) (.value <| second updatedCardsToCompare)
                    else
                        False

                updatedCanClickCards =
                    if numberComparing == 2 then
                        False
                    else
                        True
            in
                ( { model
                    | deck = updatedDeck
                    , cardsToCompare =
                        (if numberComparing /= 2 then
                            updatedCardsToCompare
                         else
                            ( blankCard, blankCard )
                        )
                    , canClickCards =
                        updatedCanClickCards
                  }
                , if numberComparing == 2 then
                    if areEqual == True then
                        restoreComparing
                    else
                        restoreComparingAndFlipped
                  else
                    Cmd.none
                )

        RestoreComparing ->
            let
                gameIsOver =
                    List.all (\card -> card.faceDown == False) model.deck
            in
                ( { model | deck = List.map (\card -> { card | comparing = False }) model.deck, canClickCards = True, gameEnd = gameIsOver }, Cmd.none )

        RestoreComparingAndFlipped ->
            let
                updatedDeck =
                    List.map
                        (\card ->
                            if card.comparing == True then
                                { card | faceDown = True }
                            else
                                card
                        )
                        model.deck
            in
                ( { model | deck = List.map (\card -> { card | comparing = False }) updatedDeck, canClickCards = True, score = model.score + 1 }, Cmd.none )


restoreComparing : Cmd Msg
restoreComparing =
    sleep 1000 |> Task.perform (\_ -> RestoreComparing)


restoreComparingAndFlipped : Cmd Msg
restoreComparingAndFlipped =
    sleep 1000 |> Task.perform (\_ -> RestoreComparingAndFlipped)


newGame : Cmd Msg
newGame =
    sleep 0 |> Task.perform (\_ -> NewGame)


resetGameEnd : Model -> Model
resetGameEnd model =
    { model | gameEnd = False }


resetScore : Model -> Model
resetScore model =
    { model | score = 0 }


flipAllCardsFaceDown : Model -> Model
flipAllCardsFaceDown model =
    { model | deck = List.map (\card -> { card | faceDown = True }) model.deck }


shuffle : Model -> Model
shuffle model =
    let
        randomNums =
            Random.step (Random.list (List.length model.deck) (Random.int 1 100)) model.seed |> first
    in
        { model | deck = List.map2 (,) randomNums model.deck |> List.sortBy first |> List.unzip |> second }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

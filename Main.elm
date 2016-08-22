module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onSubmit)
import Html.App as App


-- model


type alias Model =
    { players : List Player
    , name : String
    , playerId : Maybe Int
    , plays : List Play
    }


type alias Player =
    { id : Int
    , name : String
    , points : Int
    }


type alias Play =
    { id : Int
    , playerId : Int
    , name : String
    , points : Int
    }


initialModel : Model
initialModel =
    { players = []
    , name = ""
    , playerId = Nothing
    , plays = []
    }



-- update


type Msg
    = Edit
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | DeletePlay


update : Msg -> Model -> Model
update msg model = case msg of
        Input value ->
            { model | name = value }

        Cancel ->
            { model | name = "", playerId = Nothing }

        Save ->
            save model

        Score scorer points ->
            recordScore model scorer points

        _ ->
            model


recordScore : Model -> Player -> Int -> Model
recordScore model scorer points =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == scorer.id then
                        { player | points = player.points + points }
                    else
                        player
                )
                model.players

        play =
            Play ((List.length model.plays) + 1) scorer.id scorer.name points
    in
        { model | players = newPlayers, plays = play :: model.plays }


save : Model -> Model
save model =
    add model


add : Model -> Model
add model =
    let
        player =
            Player ((List.length model.players) + 1) model.name 0

        newPlayers =
            player :: model.players
    in
        { model
            | players = newPlayers
            , name = ""
        }



-- view


view : Model -> Html Msg
view model =
    div [ class "scoreboard" ]
        [ h1 [] [ text "Score Keeper" ]
        , playerSection model
        , playerForm model
        , p [] [ text (toString model) ]
        ]


playerSection : Model -> Html Msg
playerSection model =
    div []
        [ playerListHeader
        , playerList model
        , pointTotal model
        ]


playerList : Model -> Html Msg
playerList model =
    ul []
        (List.map player model.players)


player : Player -> Html Msg
player player =
    li []
        [ i [ class "edit" ] []
        , div [] [ text player.name ]
        , button [ type' "button", onClick (Score player 1) ] [ text "1pt" ]
        , button [ type' "button", onClick (Score player 2) ] [ text "2pt" ]
        , button [ type' "button", onClick (Score player 3) ] [ text "3pt" ]
        , div [] [ text (toString player.points) ]
        ]


pointTotal : Model -> Html Msg
pointTotal model =
    let
        total =
            List.map .points model.plays
                |> List.sum
    in
        footer []
            [ div [] [ text "Total:" ]
            , div [] [ text (toString total) ]
            ]


playerListHeader : Html Msg
playerListHeader =
    header []
        [ div [] [ text "Name" ]
        , div [] [ text "Points" ]
        ]


playerForm : Model -> Html Msg
playerForm model =
    Html.form [ onSubmit Save, class "pure-form" ]
        [ input
            [ type' "text"
            , onInput Input
            , placeholder "Add/Edit Player"
            , value model.name
            ]
            []
        , button [ type' "submit" ] [ text "Save" ]
        , button [ type' "button", onClick Cancel ] [ text "Cancel" ]
        ]


main : Program Never
main =
    App.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }

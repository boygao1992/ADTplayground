module Main exposing (..)

import Html
    exposing
        ( button
        , div
        , h1
        , input
        , text
        , textarea
        , Html
        )
import Html.Attributes
    exposing
        ( id
        , class
        , placeholder
        , rows
        , type_
        , value
        )
import Html.Events exposing (onInput)


type alias Model =
    { email : String
    , message : String
    }


initialModel : Model
initialModel =
    { email = ""
    , message = ""
    }


type Msg
    = InputEmail String
    | InputMessage String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputEmail email_ ->
            ( { model | email = String.toLower email_ }, Cmd.none )

        InputMessage message_ ->
            ( { model | message = message_ }, Cmd.none )

        Submit ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        header =
            div [] [ h1 [] [ text "Contact us" ] ]

        body =
            div []
                [ div []
                    [ input
                        [ placeholder "your email"
                        , type_ "email"
                        , onInput InputEmail
                        , value model.email
                        ]
                        []
                    ]
                , div []
                    [ textarea
                        [ placeholder "your message"
                        , rows 7
                        , onInput InputMessage
                        ]
                        []
                    ]
                ]

        footer =
            div [] [ button [] [ text "Submit" ] ]
    in
        Html.form
            []
            [ header
            , body
            , footer
            , div [] [ text <| toString <| model ]
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , subscriptions = \model -> Sub.none
        , view = view
        }

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
        , disabled
        , placeholder
        , required
        , rows
        , type_
        , value
        )
import Html.Events
    exposing
        ( onClick
        , onInput
        , onSubmit
        )
import Http
    exposing
        ( expectStringResponse
          -- jsonBody : a -> Http.Body
        , jsonBody
          -- post : String -> Http.Body -> Json.Decoder.Decoder a -> Http.Request a
          -- post url
        , post
          {-
             request :
             { method : String
             , headers : List Header
             , url : String
             , body : Body
             , expect : Expect a
             , timeout : Maybe Time
             , withCredentials : Bool
             } -> Request a
          -}
        , request
          -- send : (Result Error a -> msg) -> Request a -> Cmd msg
        , send
          {-
             type Error
                 = BadUrl String
                 | Timeout
                 | NetworkError
                 | BadStatus (Response String)
                 | BadPayload String (Response String)
          -}
        , Error
        )
import Json.Decode as Decode
import Json.Encode as Encode


type alias Model =
    { email : String
    , message : String
    , status : SubmissionStatus
    }


type SubmissionStatus
    = NotSubmitted
    | InProcess
    | Succeeded
    | Failed


initialModel : Model
initialModel =
    { email = ""
    , message = ""
    , status = NotSubmitted
    }


type Msg
    = InputEmail String
    | InputMessage String
    | Submit
    | SubmitResponse (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputEmail email_ ->
            ( { model | email = String.toLower email_ }, Cmd.none )

        InputMessage message_ ->
            ( { model | message = message_ }, Cmd.none )

        Submit ->
            ( { model | status = InProcess }, submit model )

        SubmitResponse res ->
            case res of
                Result.Ok () ->
                    ( { model
                        | status = Succeeded
                        , email = ""
                        , message = ""
                      }
                    , Cmd.none
                    )

                Result.Err _ ->
                    ( { model | status = Failed }, Cmd.none )


submit : Model -> Cmd Msg
submit model =
    let
        url =
            "http://localhost:3000/api/contact"

        json =
            Encode.object
                [ ( "email", Encode.string model.email )
                , ( "message", Encode.string model.message )
                ]

        -- 1. lazy solution: expecting JSON and transform it into ().
        -- for POST request, usually no return is expected but a Status code.
        -- a success POST with no return is denoted as Unit / ().
        -- decoder : Decode.Decoder ()
        -- decoder =
        --     -- Basics.always : a -> b -> a,
        --     -- create a function that always returns the same value.
        --     Decode.map (always ()) <| Decode.string
        -- request : Http.Request ()
        -- request =
        --     Http.post url (Http.jsonBody json) decoder
        -- 2. better solution
        request : Http.Request ()
        request =
            Http.request
                { method = "POST"
                , headers = []
                , url = url
                , body = Http.jsonBody json
                , expect = Http.expectStringResponse (\_ -> Result.Ok ())
                , timeout = Maybe.Nothing
                , withCredentials = False
                }
    in
        Http.send SubmitResponse request


view : Model -> Html Msg
view model =
    let
        header model =
            div []
                [ h1 [] [ text "Contact us" ]
                , renderStatus model.status
                ]

        renderStatus status =
            case status of
                NotSubmitted ->
                    div [] []

                InProcess ->
                    div [] [ text "Sending request..." ]

                Succeeded ->
                    div [] [ text "Request received" ]

                Failed ->
                    div [] [ text "Request failed. Please try again." ]

        body =
            div []
                [ div []
                    [ input
                        [ placeholder "your email"

                        -- utilize browser's built-in validation,
                        -- by specifying the type to `email`
                        , type_ "email"
                        , onInput InputEmail
                        , value model.email
                        , required True
                        ]
                        []
                    ]
                , div []
                    [ textarea
                        [ placeholder "your message"
                        , rows 7
                        , onInput InputMessage
                        , value model.message
                        , required True
                        ]
                        []
                    ]
                ]

        footer model =
            div []
                [ -- default behavior of any button attached to a form is to refresh the page
                  button
                    [ -- onClick Submit
                      disabled (model.status == InProcess)
                    ]
                    [ text "Submit" ]
                , button
                    [ -- default type: `submit`
                      -- rewrite the type to normal `button`
                      type_ "button"
                    ]
                    [ text "Cancel" ]
                ]
    in
        Html.form
            [ -- default button type in form is `submit`
              -- rewrite the default behavior of `submit` button which is to refresh the page
              onSubmit Submit
            ]
            [ header model
            , body
            , footer model
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

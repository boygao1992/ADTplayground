module Contact exposing (..)

import Html
    exposing
        ( button
        , div
        , h1
        , input
        , label
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
        , novalidate
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
import Json.Encode as Encode
import Validation
    exposing
        ( customizeErr
        , displayValue
        , extractError
        , isNotEmpty
        , isEmail
        , isInt
        , isNatural
        , validate
        , (>=>)
        , (<*>)
        , pure
        , Field
            ( NotValidated
            , Valid
            , Invalid
            )
        , OptionalField
        )


type alias Email =
    String


type alias Password =
    String


type alias ErrMsg =
    String


type alias Model =
    { email : Field Email
    , password : Field Password
    , confirmPassword : Field Password
    , acceptPolicy : Field Bool
    , status : SubmissionStatus
    }


type SubmissionStatus
    = NotSubmitted
    | InProcess
    | Succeeded
    | Failed


initialModel : Model
initialModel =
    { email = NotValidated ""
    , password = NotValidated ""
    , confirmPassword = NotValidated ""
    , acceptPolicy = NotValidated False
    , status = NotSubmitted
    }


type Msg
    = InputEmail Email
    | InputPassword Password
    | InputConfirmPassword Password
    | CheckAcceptPolicy Bool
    | Submit
    | SubmitResponse (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputEmail email_ ->
            ( { model | email = NotValidated <| String.toLower email_ }, Cmd.none )

        InputPassword password_ ->
            ( { model | password = NotValidated password_ }, Cmd.none )

        InputConfirmPassword confirmPassword_ ->
            ( { model | confirmPassword = NotValidated confirmPassword_ }, Cmd.none )

        Submit ->
            model |> validateModel |> submitIfValid

        SubmitResponse res ->
            case res of
                Result.Ok () ->
                    ( { initialModel
                        | status = Succeeded
                      }
                    , Cmd.none
                    )

                Result.Err _ ->
                    ( { model | status = Failed }, Cmd.none )


validateModel : Model -> Model
validateModel model =
    let
        -- customize error messages
        emailValidation =
            customizeErr "An email is required" isNotEmpty
                >=> customizeErr "Please ensure this is a valid email" isEmail

        passwordValidation =
            customizeErr "A password is required" isNotEmpty

        confirmPasswordValidation =
            customizeErr "A password is required" isNotEmpty
    in
        { model
            | email = model.email |> validate emailValidation
            , password = model.password |> validate passwordValidation
            , confirmPassword = model.confirmPassword |> validate confirmPasswordValidation

            -- , acceptPolicy = model.acceptPolicy |>
        }


submitIfValid : Model -> ( Model, Cmd Msg )
submitIfValid model =
    let
        submissionResult : Field (Cmd Msg)
        submissionResult =
            (Validation.pure submit)
                <*> model.email
                <*> model.password
                <*> model.confirmPassword
                <*> model.acceptPolicy
    in
        case submissionResult of
            Valid cmd ->
                ( { model | status = InProcess }, cmd )

            _ ->
                ( model, Cmd.none )


submit : Email -> Password -> Password -> Bool -> Cmd Msg
submit email password _ _ =
    let
        url =
            "http://localhost:3000/api/contact"

        json =
            Encode.object
                [ ( "email", Encode.string email )
                , ( "password", Encode.string password )
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
                [ h1 [] [ text "Register" ]
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

        errorLabel : Field a -> Html msg
        errorLabel field =
            label
                [ class "label label-error" ]
                [ field
                    |> extractError
                    |> Maybe.withDefault ""
                    |> text
                ]

        body =
            div []
                [ div []
                    [ input
                        [ placeholder "your email *"

                        -- utilize browser's built-in validation,
                        -- by specifying the type to `email`
                        , type_ "email"
                        , onInput InputEmail
                        , value <| displayValue identity model.email
                        , required True
                        ]
                        []
                    , errorLabel model.email
                    ]
                , div []
                    [ textarea
                        [ placeholder "your password *"
                        , type_ "password"
                        , onInput InputPassword
                        , value (model.password |> rawValue)
                        , required True
                        ]
                        []
                    , errorLabel model.message
                    ]
                , div []
                    [ textarea
                        [ placeholder "your age"
                        , onInput Input
                        , value <|
                            displayValue
                                (Maybe.withDefault "" << Maybe.map toString)
                            <|
                                model.age
                        ]
                        []
                    , errorLabel model.age
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

            -- Prevent browser's built-in field validation functionality
            , Html.Attributes.novalidate True
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

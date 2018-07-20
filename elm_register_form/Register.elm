module Register exposing (..)

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
        , onCheck
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
        , extractError
        , isNotEmpty
        , isEmail
        , isInt
        , isNatural
        , isTrue
        , isEqualTo
        , validate
        , validateAlways
        , (>=>)
        , (<*>)
        , pure
        , field
        , getRaw
        , getValidity
        , Field
        , OptionalField
        , Validity(NotValidated, Valid, Invalid)
        , Validator
        )


{--- Model ---}


type alias Email =
    String


type alias Password =
    String


type alias Age =
    Int


type alias PolicyAcceptance =
    Bool


type alias ErrMsg =
    String


type alias InputField a =
    Field String a


type alias OptionalInputField a =
    OptionalField String a


type alias Checkbox a =
    Field Bool a


type SubmissionStatus
    = NotSubmitted
    | InProcess
    | Succeeded
    | Failed


type alias Model =
    { email : InputField Email
    , password : InputField Password
    , confirmPassword : InputField Password
    , age : OptionalInputField Age
    , acceptPolicy : Checkbox PolicyAcceptance
    , status : SubmissionStatus
    , attemptedOnce : Bool
    }


initialModel : Model
initialModel =
    { email = field ""
    , password = field ""
    , confirmPassword = field ""
    , age = field ""
    , acceptPolicy = field False
    , status = NotSubmitted
    , attemptedOnce = False
    }


isStrongPassword : Validator String Password
isStrongPassword password =
    if String.length password >= 6 then
        Result.Ok password
    else
        Result.Err "Your password is not strong enough"


emailValidator : Validator String Email
emailValidator =
    customizeErr "An email is required" isNotEmpty
        >=> customizeErr "Please ensure this is a valid email" isEmail


passwordValidator : Validator String Password
passwordValidator =
    customizeErr "Please enter a password" isNotEmpty
        >=> isStrongPassword


confirmPasswordValidator : InputField Password -> Validator String Password
confirmPasswordValidator validatedPassword =
    customizeErr "Please retype your password" isNotEmpty
        >=> customizeErr "The passwords don't match" (isEqualTo validatedPassword)


ageValidator : Validator String (Maybe Age)
ageValidator =
    Validation.optional isNatural


acceptPolicyValidator : Validator Bool PolicyAcceptance
acceptPolicyValidator =
    customizeErr "You must accept the policy" isTrue



{--- Msg ---}


type Msg
    = InputEmail String
    | InputPassword String
    | InputConfirmPassword String
    | InputAge String
    | CheckAcceptPolicy Bool
    | Submit
    | SubmitResponse (Result Http.Error ())



{--- Update ---}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputEmail email_ ->
            let
                e =
                    email_
                        |> String.toLower
                        |> field
                        |> if model.attemptedOnce then
                            validateAlways emailValidator
                           else
                            identity
            in
                ( { model
                    | email = e
                  }
                , Cmd.none
                )

        InputPassword password_ ->
            let
                p =
                    password_
                        |> field
                        |> if model.attemptedOnce then
                            validateAlways passwordValidator
                           else
                            identity

                cp =
                    model.confirmPassword
                        |> if model.attemptedOnce && Validation.isValid p then
                            validateAlways (confirmPasswordValidator p)
                           else
                            Validation.reset
            in
                ( { model
                    | password = p
                    , confirmPassword = cp

                    -- Important:
                    -- validation of confirmPassword depends on password
                    -- reset the validity of confirmPassword, if password changes
                    -- , confirmPassword = Validation.reset model.confirmPassword
                  }
                , Cmd.none
                )

        InputConfirmPassword confirmPassword_ ->
            let
                p =
                    model.password

                cp =
                    confirmPassword_
                        |> field
                        |> if model.attemptedOnce && Validation.isValid p then
                            validateAlways (confirmPasswordValidator p)
                           else
                            identity
            in
                ( { model | confirmPassword = cp }, Cmd.none )

        InputAge age_ ->
            let
                a =
                    age_
                        |> field
                        |> if model.attemptedOnce then
                            validateAlways ageValidator
                           else
                            identity
            in
                ( { model | age = a }, Cmd.none )

        CheckAcceptPolicy acceptPolicy_ ->
            let
                ap =
                    acceptPolicy_
                        |> field
                        |> if model.attemptedOnce then
                            validateAlways acceptPolicyValidator
                           else
                            identity
            in
                ( { model | acceptPolicy = ap }, Cmd.none )

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
        -- email
        validatedEmail =
            model.email |> validate emailValidator

        -- password
        validatedPassword =
            model.password |> validate passwordValidator

        -- confirmPassword
        validatedConfirmPassword =
            model.confirmPassword |> validate (confirmPasswordValidator validatedPassword)

        -- age
        validatedAge =
            model.age |> validate ageValidator

        -- acceptPolicy
        validatedAcceptPolicy =
            model.acceptPolicy |> validate acceptPolicyValidator
    in
        { model
            | email = validatedEmail
            , password = validatedPassword
            , confirmPassword = validatedConfirmPassword
            , age = validatedAge
            , acceptPolicy = validatedAcceptPolicy
        }


submitIfValid : Model -> ( Model, Cmd Msg )
submitIfValid model =
    let
        submissionResult : Validity (Cmd Msg)
        submissionResult =
            (Validation.pure submit)
                <*> (getValidity model.email)
                <*> (getValidity model.password)
                <*> (getValidity model.confirmPassword)
                <*> (getValidity model.age)
                <*> (getValidity model.acceptPolicy)
    in
        case submissionResult of
            Valid cmd ->
                ( { model | status = InProcess }, cmd )

            _ ->
                ( { model | attemptedOnce = True }, Cmd.none )



-- TODO: not supposed to manually coordinate optional fields here, Maybe Age


submit : Email -> Password -> Password -> Maybe Age -> PolicyAcceptance -> Cmd Msg
submit email password _ age _ =
    let
        url =
            "http://localhost:3000/api/contact"

        json =
            Encode.object
                [ ( "email", Encode.string email )
                , ( "password", Encode.string password )
                , ( "age"
                  , age
                        |> Maybe.map Encode.int
                        |> Maybe.withDefault Encode.null
                  )
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



{--- View ---}


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

        errorLabel : Field raw a -> Html msg
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
                        , value <| getRaw <| model.email
                        , required True
                        ]
                        []
                    , errorLabel model.email
                    ]
                , div []
                    [ input
                        [ type_ "password"
                        , placeholder "enter your password *"
                        , onInput InputPassword
                        , value <| getRaw <| model.password
                        , required True
                        ]
                        []
                    , errorLabel model.password
                    ]
                , div []
                    [ input
                        [ type_ "password"
                        , placeholder "repeat your password *"
                        , onInput InputConfirmPassword
                        , value <| getRaw <| model.confirmPassword
                        , required True
                        ]
                        []
                    , errorLabel model.confirmPassword
                    ]
                , div []
                    [ input
                        [ placeholder "your age"
                        , onInput InputAge
                        , value <| getRaw <| model.age
                        ]
                        []
                    , errorLabel model.age
                    ]
                , div []
                    [ input
                        [ type_ "checkbox"
                        , onCheck CheckAcceptPolicy
                        , value <| toString <| getRaw <| model.acceptPolicy
                        , required True
                        ]
                        []
                    , label [] [ text "I accept the private policy" ]
                    ]
                , div [] [ errorLabel model.acceptPolicy ]
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



{--- Main ---}


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , subscriptions = \model -> Sub.none
        , view = view
        }

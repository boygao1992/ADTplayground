module Main exposing (..)

import Dict
import Html as H
import Html.Attributes as A
import Html.Events as E


-- Component decorator to distinguish different instances of the same type


type alias Id =
    String


type alias WithId a =
    { id : Id
    , value : a
    }


getId : WithId a -> Id
getId { id } =
    id


getValue : WithId a -> a
getValue { value } =
    value


withId : Id -> a -> WithId a
withId id a =
    { id = id, value = a }



-- Component 1: Button


type alias ButtonModel =
    { active : Bool }


type ButtonMsg
    = Pressed


buttonUpdate : ButtonMsg -> ButtonModel -> ( ButtonModel, Cmd msg )
buttonUpdate msg model =
    case msg of
        Pressed ->
            ( { model | active = not model.active }, Cmd.none )


buttonView : String -> ButtonModel -> H.Html ButtonMsg
buttonView label model =
    H.button
        [ A.disabled (model.active == False)
        , E.onClick Pressed
        ]
        [ H.text label ]



-- Component 2: InputField


type alias InputFieldModel =
    { input : String }


type InputFieldMsg
    = KeyInput String


inputFieldUpdate : InputFieldMsg -> InputFieldModel -> ( InputFieldModel, Cmd msg )
inputFieldUpdate msg model =
    case msg of
        KeyInput s ->
            ( { model | input = s }, Cmd.none )


inputFieldView : String -> InputFieldModel -> H.Html InputFieldMsg
inputFieldView default model =
    H.input
        [ A.placeholder default
        , E.onInput KeyInput
        ]
        []



-- Constraint 1: Only one Button is active at anytime


type alias Constraint1Model =
    -- state history / buffer
    Dict.Dict Id ButtonModel


type Constraint1Msg
    = -- listen to state updates
      Constraint1Msg Id ButtonModel



-- Constraint 2: Only one InputField is not empty


type alias Constraint2Model =
    Dict.Dict Id InputFieldModel


type Constraint2Msg
    = Constraint2Msg Id InputFieldModel



-- All Children together


type ChildModel
    = ChildModelButton (WithId ButtonModel)
    | ChildModelInputField (WithId InputFieldModel)


type ChildMsg
    = ChildMsgButton (WithId ButtonMsg)
    | ChildMsgInputField (WithId InputFieldMsg)


childModelView : ChildModel -> H.Html ChildMsg
childModelView child =
    case child of
        ChildModelButton b ->
            buttonView (getId b) (getValue b)
                |> H.map (ChildMsgButton << (withId (getId b)))

        ChildModelInputField i ->
            inputFieldView (getId i) (getValue i)
                |> H.map (ChildMsgInputField << (withId (getId i)))



-- All Constraints together


type alias ConstraintModel =
    { constraint1 : Constraint1Model
    , constraint2 : Constraint2Model
    }


type ConstraintMsg
    = ConstraintMsgConstraint1 Constraint1Msg
    | ConstraintMsgConstraint2 Constraint2Msg



-- Container


type ComponentModel
    = Widget ChildModel
    | Constraint ConstraintModel


type alias Model =
    List ComponentModel


type Msg
    = MsgChild ChildMsg
    | MsgConstraint ConstraintMsg


initialModel : Model
initialModel =
    [ Widget <| ChildModelButton <| withId "1" <| { active = True }
    , Widget <| ChildModelButton <| withId "2" <| { active = False }
    , Widget <| ChildModelButton <| withId "3" <| { active = False }
    , Widget <| ChildModelInputField <| withId "4" <| { input = "" }
    , Widget <| ChildModelInputField <| withId "5" <| { input = "" }
    ]


update : Msg -> Model -> ( Model, Cmd msg )
update _ model =
    ( model, Cmd.none )


dispatchView : ComponentModel -> H.Html Msg
dispatchView component =
    case component of
        Widget child ->
            childModelView child
                |> H.map MsgChild

        Constraint _ ->
            H.text ""


view : Model -> H.Html Msg
view model =
    H.div [] (model |> List.map dispatchView)


main : Program Never Model Msg
main =
    H.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , subscriptions = \model -> Sub.none
        , view = view
        }

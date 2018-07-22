module Main exposing (..)

import Dict
import Html as H
import Html.Attributes as A


-- Component decorator to distinguish different instances of the same type


type alias Id =
    String


type alias WithId a =
    { a | id : Id }



-- Component 1


type alias ButtonModel =
    { active : Bool }


type ButtonMsg
    = Pressed



-- Component 2


type alias InputFieldModel =
    { input : String }


type InputFieldMsg
    = KeyInput String



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



-- All Constraint together


type alias ConstraintModel =
    { constraint1 : Constraint1Model
    , constraint2 : Constraint2Model
    }


type ConstraintMsg
    = ConstraintMsgConstraint1 Constraint1Msg
    | ConstraintMsgConstraint2 Constraint2Msg



-- Container


type alias Model =
    { children : List ChildModel }


type Msg
    = MsgChild ChildMsg
    | MsgConstraint ConstraintMsg


initialModel : Model
initialModel =
    {}


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , subscriptions = \model -> Sub.none
        , view = view
        }

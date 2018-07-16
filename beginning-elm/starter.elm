module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


{- 3. Write functions and Type Signatures in Elm
   politely : String -> String
   politely phrase =
       "Excuse me, " ++ phrase


   ask : String -> String -> String
   ask thing place =
       "is there a "
           ++ thing
           ++ " in the "
           ++ place
           ++ "?"


   askPolitelyAboutFish : String -> String
   askPolitelyAboutFish =
       politely << (ask "fish")


   main : Html.Html msg
   main =
       text <| askPolitelyAboutFish "sock"
-}
{- 4. Store key-value pairs using Records in Elm
   type alias Dog =
       { name : String
       , age : Int
       }


   dog : Dog
   dog =
       { name = "Spock"
       , age = 3
       }



   -- main =
   --     text <| toString <| .age dog


   renderDog : Dog -> String
   renderDog dog =
       dog.name ++ ", " ++ toString dog.age


   main : Html.Html msg
   main =
       text <| renderDog dog
-}
{- 5. Sequencing Data with Lists in Elm
   foo : List String
   foo =
       [ "one", "two", "three" ]


   type alias Name =
       String


   type alias Age =
       Int


   type alias Person =
       { name : Name
       , age : Age
       }


   people : List Person
   people =
       [ { name = "Legolas", age = 2931 }
       , { name = "Gimli", age = 139 }
       ]


   getName : Person -> Name
   getName peep =
       .name peep


   names : List Person -> List Name
   names peeps =
       List.map getName peeps



   -- main : Html.Html msg
   -- main =
   --     text <| toString <| names people


   firstPerson : Name -> List Person -> Maybe Person
   firstPerson name peeps =
       -- foldl :: (a -> b -> b) -> b -> List a -> b
       List.foldl
           -- reducer :: First Monoid b => a -> Maybe b -> Maybe b
           (\peep memo ->
               -- Monoid concatenation
               case memo of
                   Just _ ->
                       memo

                   Nothing ->
                       -- lift `peep`: Person into a First Monoid
                       if getName peep == name then
                           Just peep
                       else
                           Nothing
           )
           Maybe.Nothing
           peeps


   main : Html.Html msg
   main =
       text <| toString <| firstPerson "Legolas" people
-}
{- 6. Render HTML in the browser using the Html module in Elm

   type alias Ship =
       { name : String
       , model : Maybe String
       , cost : Int
       }


   ships : List Ship
   ships =
       [ { name = "X-wing", model = Nothing, cost = 1499999 }
       , { name = "Millennium Falcon", model = Nothing, cost = 100000 }
       , { name = "Death Star", model = Nothing, cost = 1000000000 }
       ]


   renderShip : Ship -> Html msg
   renderShip ship =
       li []
           [ text ship.name
           , text ", "
           , b [] [ text <| toString ship.cost ]
           ]


   renderShips : List Ship -> Html msg
   renderShips ships =
       -- div :: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
       div
           [ style
               [ ( "font-family", "-apple-system" )
               , ( "padding", "1em" )
               ]
           ]
           [ h1 [] [ text "Ships" ]
           , ul [] (List.map renderShip ships)
           ]


   main : Html msg
   main =
       renderShips ships
-}
{- 7. Reuse functions through type variables in Elm

   numbers =
       [ 1, 2, 3, 4, 5 ]


   fruits =
       [ { name = "Orange" }
       , { name = "Banana" }
       ]


   printThing : a -> Html msg
   printThing x =
       li []
           [ text <|
               -- toString :: a -> String (polymorphism)
               toString x
           ]


   main =
       ul [] (List.map printThing numbers)
-}
{- 8. Create apps using the Elm application architecture -}
-- Model / State


type alias Model =
    { showFace : Bool }


model =
    { showFace = False }



-- Msg / Event


type Msg
    = ShowFace



-- Update / State transition function


update msg model =
    case msg of
        ShowFace ->
            { model | showFace = True }



-- View


renderFace : Bool -> Html msg
renderFace show =
    if show then
        text "ᕕ( ᐛ )ᕗ"
    else
        text ""


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Face generator" ]
        , button [ onClick ShowFace ] [ text "Face me" ]
        , div [] [ renderFace model.showFace ]
        ]


main : Program Never Model Msg
main =
    beginnerProgram
        { model = model
        , update = update
        , view = view
        }

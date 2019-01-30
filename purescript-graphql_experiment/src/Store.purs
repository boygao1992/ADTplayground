module Store where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Foldable (class Foldable, find)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

type Id = String

type Message =
  { id :: Id
  , content :: Maybe String
  }

type Store =
  { messages :: Ref.Ref (Array Message)}

createStore :: Aff Store
createStore = do
  messages <- liftEffect
              $ Ref.new [ { id: "001", content: Nothing }
                        , { id: "002", content: Just "hello world" }
                        ]
  pure { messages }


findById
  :: forall r f
   . Foldable f
  => Id
  -> f { id :: Id | r }
  -> Maybe { id :: Id | r}
findById id = find ( (_ == id) <<< _.id )

readMessageById :: forall r. { messages :: Ref.Ref (Array Message) | r } -> Id -> Aff (Maybe Message)
readMessageById { messages: msRef } id = do
  messages <- liftEffect $ Ref.read msRef
  pure $ findById id $ messages

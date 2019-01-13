module Store where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Foldable (class Foldable, find)
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

type Id = String

newtype Message = Message
  { id :: Id
  , next_id :: Id
  , next :: Maybe Message
  }
derive instance newtypeMessage :: Newtype Message _

type Store =
  { messages :: Ref.Ref (Array Message)}

createStore :: Aff Store
createStore = do
  messages <- liftEffect
              $ Ref.new [ Message { id: "001", next_id : "002", next : Nothing }
                        , Message { id: "002", next_id : "001", next : Nothing }
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
  pure $ map wrap <<< findById id <<< map unwrap $ messages

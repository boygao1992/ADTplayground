module Main where

import Prelude
import Effect (Effect)

import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic, Constructor, Argument)
import GraphQL.Type.Internal (GraphQLType)
import Type.Proxy (Proxy(..))
import Type.Row (RProxy(..))
import GraphQL.Type.Internal.ToObjectTypeField (class FetchScalarFields, class ToFieldList, toObject)
import ForumExample.Model (Comment, Post, User)
import Prim.RowList as RowList
import Effect.Aff (Aff)
import Data.Nullable (Nullable)

toFieldType
  :: forall spec specName specRow specRl specFl source
   . Generic spec (Constructor specName (Argument (Record specRow)))
  => RowList.RowToList specRow specRl
  => ToFieldList specRl specFl
  => FetchScalarFields specFl source
  => Proxy spec
  -> RProxy source
toFieldType _ = RProxy :: RProxy source

toFieldTypeText :: RProxy
  ( id :: String
  )
toFieldTypeText = toFieldType (Proxy :: Proxy User)

-- TODO testing
toObjectTest ::
  { "Comment" :: Unit -> Nullable (GraphQLType Comment)
  , "Post" :: Unit -> Nullable (GraphQLType Post)
  }
  -> GraphQLType User
toObjectTest =
  toObject
    (Proxy :: Proxy User)
    { id: Nothing
    , posts: \({ args: { date } }) -> pure []
    , comments: \({ args: { limit } }) -> pure []
    }


main :: Effect Unit
main = do
  pure unit

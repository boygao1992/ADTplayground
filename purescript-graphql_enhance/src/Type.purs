module Type where

import Prelude

import Control.Promise (Promise)
import Data.Function.Uncurried (Fn2, Fn3, Fn4)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Effect (Effect)

-- | Schema
data Schema ctx a

-- | Object
data ObjectType ctx a
data ObjectTypeField ctx a
data ObjectTypeFieldArg a

-- | InputObject
data InputObjectType a
data InputObjectTypeField a

-- | Scalar
data ScalarType a

-- | Enum
data EnumType a

newtype EnumValue a = EnumValue
  { name :: String
  , description :: Nullable String
  , value :: a
  }

-- | List
data ListType t a

foreign import boolean :: ScalarType (Maybe Boolean)
foreign import int :: ScalarType (Maybe Int)
foreign import float :: ScalarType (Maybe Number)
foreign import string :: ScalarType (Maybe String)
foreign import id :: ScalarType (Maybe String)

foreign import _schema :: forall a ctx.
  Fn2
    (ObjectType ctx (Maybe a))
    (Nullable (ObjectType ctx (Maybe a)))
    (Schema ctx a)

-- foreign import list :: forall t a.
--   GraphQLType (t a) => t a -> ListType (t a) (Maybe (Array a))

foreign import _enumType :: forall a.
  Fn3 String (Nullable String) (Array (EnumValue a)) (EnumType (Maybe a))

foreign import _objectType :: forall a ctx fields.
  Fn3 String (Nullable String) (Record fields) (ObjectType ctx a)

foreign import _field :: ∀ z t a b decl args ctx.
  Fn2
    (Maybe z -> Nullable z)
    (Nullable z -> Maybe z)
    (Fn4
      (t b)
      (Nullable String)
      decl
      (a -> args -> ctx -> Effect (Promise b))
      (ObjectTypeField ctx a)
    )

foreign import _argument :: ∀ t a.
  Fn2 (t a) (Nullable String) (ObjectTypeFieldArg a)

foreign import _inputObjectType :: ∀ a r.
  Fn3 String (Nullable String) (Record r) (InputObjectType (Maybe a))

foreign import _inputField :: ∀ t a.
  Fn2 (t a) (Nullable String) (InputObjectTypeField a)

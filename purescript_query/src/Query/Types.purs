module Query.Types where

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Map (Map)

{-
Relational Constraints
1. Not Null
2. Unique

(Not Null, Unique) , (Not Null, Unique) |        One to One        |       Identity , Identity
(Not Null, Unique) , (Not Null)         |        One to OneOrMore  | NonEmpty Array , Identity
(Not Null, Unique) , (Unique)           |        One to ZeroOrOne  |          Maybe , Identity
(Not Null, Unique) , ()                 |        One to ZeroOrMore |          Array , Identity
(Not Null)         , (Not Null, Unique) |  OneOrMore to One        |       Identity , NonEmpty Array
(Not Null)         , (Not Null)         |  OneOrMore to OneOrMore  | NonEmpty Array , NonEmpty Array TODO
(Not Null)         , (Unique)           |  OneOrMore to ZeroOrOne  |          Maybe , NonEmpty Array TODO
(Not Null)         , ()                 |  OneOrMore to ZeroOrMore |          Array , NonEmpty Array TODO
(Unique)           , (Not Null, Unique) |  ZeroOrOne to One        |       Identity , Maybe
(Unique)           , (Not Null)         |  ZeroOrOne to OneOrMore  | NonEmpty Array , Maybe          TODO
(Unique)           , (Unique)           |  ZeroOrOne to ZeroOrOne  |          Maybe , Maybe          TODO
(Unique)           , ()                 |  ZeroOrOne to ZeroOrMore |          Array , Maybe          TODO
()                 , (Not Null, Unique) | ZeroOrMore to One        |       Identity , Array
()                 , (Not Null)         | ZeroOrMore to OneOrMore  | NonEmpty Array , Array          TODO
()                 , (Unique)           | ZeroOrMore to ZeroOrOne  |          Maybe , Array          TODO
()                 , ()                 | ZeroOrMore to ZeroOrMore |          Array , Array          TODO

relation towards another entity is embedded in the target entity's table with a foreign key constraint

-}

type Id = String

newtype Person = Person
  { id :: Id
  , address :: Array Address
  }

newtype Address = Address
  { id :: Id
  , person :: Maybe Person
  }

newtype Person_Address1 = Person_Address1
  ( Array (Tuple Id Id) )
newtype Person_Address2 = Person_Address2
  ( Map Id (Array Id) )

{- Product {1, 2, 3} x {A, B, C}
1 A
1 B
1 C
2 A
2 B
2 C
3 A
3 B
3 C

1 {A, B, C}
2 {A, B, C}
3 {A, B, C}

{1,2,3} A
{1,2,3} B
{1,2,3} C
-}

{- Exponential {1, 2, 3} -> {A, B, C}
1 A
2 B
3 B

{1}    A
{2, 3} B
{}     C

1 A
2 B
3 C

{1} A
{2} B
{3} C
-}

{- Exponential {1, 2, 3} <- {A, B, C}
1 A
2 B
2 C

1 {A}
2 {B, C}
3 {}

1 A
2 B
3 C

1 {A}
2 {B}
3 {C}
-}

{- Isomorphism {1, 2, 3} <-> {A, B, C}
1 A
2 B
3 C
-}

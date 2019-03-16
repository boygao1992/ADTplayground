module Run.Generic.Rep where

import Data.Functor.Variant (FProxy)
import Data.Generic.Rep (class Generic, Argument, Constructor, NoArguments, Product(..), Sum)
import Record.Builder (Builder)
import Record.Builder as Builder

{- ToOperators

NOTE input query Functor
data Talk next
  = Speak String Int next
  | Listen (String -> Int -> next)
derive instance genericTalk :: Generic (Talk next) _

Sum
  ( Constructor "Speak"
      ( Product
          (Argument String)
          ( Product
              (Argument Int)
              (Argument next)
          )
      )
  )
  (Constructor "Listen" (Argument (String -> Int -> next)))

type TALK = FProxy TalkF

_talk = SProxy :: SProxy "talk"

speak :: forall r. String -> Int -> Run (talk :: TALK | r) Unit
speak str int = Run.lift _talk (Speak str int unit)

listen :: forall r. Run (talk :: TALK | r) (String /\ Int)
listen = Run.lift _talk (Listen (/\))

NOTE output Record
{ speak :: forall r. String -> Run (talk :: FProxy Talk | r) Unit
, listen :: forall r. Run (talk :: FProxy Talk | r) (String /\ Int)
}


NOTE Patterns
1. nested products -> higher-order function
2. higher-order function -> nested product


maybe enforce nested products to be a single Record?

[Comonads as Spaces](https://blog.functorial.com/posts/2016-08-07-Comonads-As-Spaces.html)
> Functors and Pairings
> Pairings of Monads and Comonads

-}

class ToOperators (queryF :: Type -> Type) (o :: # Type) | queryF -> o
  where
    toOperators :: FProxy queryF -> Record o


class GenericToOperators rep (from :: # Type) (to :: # Type) | rep from -> to
  where
    genericToOperators :: rep -> Builder (Record from) (Record to)

-- class GenericArgumentToOperator rep

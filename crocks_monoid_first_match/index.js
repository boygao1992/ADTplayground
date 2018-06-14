const log = require( './lib/log' )

const R = require( 'ramda' )

const {
  // ADT
  First,
  // Maybe
  option, // a -> Maybe a -> a
  safe, // (a -> Boolean) -> a -> Maybe b
  // Combinators
  compose, // ((y -> z), ..., (a -> b)) -> a -> z
  constant, // a -> () -> a
  flip, // (a -> b -> c) -> b -> a -> c
  // Helper
  curry, // ((a,b,...) -> z) -> a -> b -> ... -> z
  // Logic
  and, // ((a -> Boolean) | Pred a) -> ((a -> Boolean) | Pred a) -> a -> Boolean
  not, // ((a -> Boolean) | Pred) -> a -> Boolean
  // Pointfree
  map, // Functor m => (a -> b) -> m a -> m b
  mreduceMap, // mreduceMap :: Monoid m, Foldable f => m -> (b -> a) -> f b -> a
} = require( 'crocks' )

// gt := b: Number -> a: Number -> (a > b): Boolean
const gt = flip( R.gt )
// lte := b: Number -> a: Number -> (a <= b): Boolean
const lte = flip( R.lte )

// taggedPred a b :: (b, (a -> Boolean))
// taggedPreds :: [ taggedPred Number String ]
const taggedPreds = [
  [ 'Small', and( gt( -1 ), lte( 50 ) ) ],
  [ 'Large', gt( 100 ) ],
  [ 'Medium', gt( 50 ) ],
]

// tagValue :: taggedPred a b -> a -> Maybe b
const tagValue = curry( ( [ tag, pred ] ) =>
  compose( map( constant( tag ) ), safe( pred ) )
)

// match :: [ taggedPreds a b ] -> a -> Maybe b
const match = flip( x =>
  mreduceMap( First, flip( tagValue, x ) )
)

// matchNumber :: Number -> Maybe String
const matchNumber =
  match( taggedPreds )

log(
  matchNumber( 150 )
)

log(
  matchNumber( 60 )
)

log(
  matchNumber( 30 )
)

log(
  matchNumber( -1 )
)

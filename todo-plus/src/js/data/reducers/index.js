// ADT
import First from 'crocks/First'
import State from 'crocks/State'
// Maybe
//safeLift :: ((c -> Boolean) | Pred) -> (a -> b) -> c -> Maybe b
import safeLift from 'crocks/Maybe/safeLift'
// Combinator
import applyTo from 'crocks/combinators/applyTo' // a -> (a -> b) -> b
// Predicate
import isSameType from 'crocks/predicates/isSameType'
// Helper
// mconcatMap :: Monoid m, Foldable f => m -> (b -> a) -> f b -> m a
import mconcatMap from 'crocks/helpers/mconcatMap'

// Reducer
import todo from './todo'

const fn = action =>
  safeLift(isSameType(State), applyTo(action))

const combineReducers = reducers => action =>
  mconcatMap(First, fn, reducers)
    .option(State.of(action))

const reducers = combineReducers([

])

const reducer = (prevState, action) =>
  reducer(action).execWith(prevState)

export default reducer

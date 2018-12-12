module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Symbol (SProxy(..))
import Type.Row (RProxy(..), type (+))
import Data.Variant (Variant, inj, on, default)

_foo = SProxy :: SProxy "foo"
_bar = SProxy :: SProxy "bar"
_baz = SProxy :: SProxy "baz"

type Foo r = (foo :: Int | r)
type Bar r = (bar :: Boolean | r)
type Baz r = (baz :: String | r)

type FooBar r = ( Foo + Bar + r )

someFoo :: forall r. Variant (Foo r)
someFoo = inj _foo 42

someBar :: forall r. Variant (Bar r)
someBar = inj _bar true

someBaz :: forall r. Variant (Baz r)
someBaz = inj _baz "Baz"

fooToString :: forall r. (Variant r -> String) -> Variant (Foo r) -> String
fooToString = on _foo (("Foo: " <> _) <<< show)

foobarToString :: forall r. Variant (FooBar r) -> String
foobarToString =
      on _bar (("Bar: " <> _) <<< if _ then "true" else "false")
  <<< on _foo (("Foo: " <> _) <<< show)
    $ default "not foo nor bar"

main :: Effect Unit
main = do
  log $ foobarToString someFoo
  log $ foobarToString someBar
  log $ foobarToString someBaz

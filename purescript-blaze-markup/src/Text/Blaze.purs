module Text.Blaze where

import Prelude
import Data.Exists (Exists, mkExists, runExists)

type StaticString = String

data ChoiceString
  = Static StaticString
  | String String
  -- | Text
  -- | ByteString
  | PreEscaped ChoiceString
  | External ChoiceString
  | AppendChoiceString ChoiceString ChoiceString
  | EmptyChoiceString

type Markup
  = MarkupM Unit

data MarkupM a
  = Parent { tag :: StaticString, openTag :: StaticString, endTag :: StaticString } (MarkupM a)
  | CustomParent ChoiceString (MarkupM a)
  | Leaf { tag :: StaticString, openTag :: StaticString, endTag :: StaticString } a
  | CustomLeaf ChoiceString Boolean a
  | Content ChoiceString a
  | Comment ChoiceString a
  | Append (Append a)
  | AddAttribute { rawKey :: StaticString, key :: StaticString, value :: ChoiceString } (MarkupM a)
  | AddCustomAttribute ChoiceString ChoiceString (MarkupM a)
  | Empty a

type Append a
  = Exists (AppendF a)

data AppendF a b
  = AppendF (MarkupM b) (MarkupM a)

append_ :: forall a b. MarkupM b -> MarkupM a -> MarkupM a
append_ x y = Append (mkExists (AppendF x y))

markupValue :: forall a. MarkupM a -> a
markupValue = go
  where
    go :: MarkupM a -> a
    go = case _ of
      Parent _ m1 -> go m1
      CustomParent _ m1 -> go m1
      Leaf _ x -> x
      CustomLeaf _ _ x -> x
      Content _ x -> x
      Comment _ x -> x
      Append e -> runExists (\(AppendF x y) -> go y) e
      AddAttribute _ m1 -> go m1
      AddCustomAttribute _ _ m1 -> go m1
      Empty x -> x

instance semigroupMarkupM :: Semigroup a => Semigroup (MarkupM a) where
  append = append_

instance monoidMarkupM :: Monoid a => Monoid (MarkupM a) where
  mempty = Empty mempty

instance functorMarkupM :: Functor MarkupM where
  map f x = append_ x (Empty (f (markupValue x)))

instance applyMarkupM :: Apply MarkupM where
  apply x y = append_ (append_ x y) (Empty (markupValue x (markupValue y)))

instance applicativeMarkupM :: Applicative MarkupM where
  pure = Empty

instance bindMarkupM :: Bind MarkupM where
  bind mx f = append_ mx (f (markupValue mx))

instance monadMarkupM :: Monad MarkupM

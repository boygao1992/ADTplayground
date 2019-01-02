module Symbol.Regex where

import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol
import Symbol.Utils (class ReverseSymbol)
import Prim.TypeError (class Fail, Text)

-- kind Token
-- toToken :: Char -> Token
-- toToken '{' = NumFixedLeftToken
-- toToken '}' = NumFixedRightToken
-- toToken '?' = MaybeToken
-- toToken '+' = NumPositiveToken
-- toToken '.' = AnyToken
-- toToken '*' = StarToken
-- toToken c = LitToken c

-- data Pattern
--   = CharNumFixed Char Int
--   | CharNumMaybe Char
--   | CharNumPositive Char
--   | CharStar Char

foreign import kind Pattern
foreign import data CharNumFixed :: Symbol -> Symbol -> Pattern
foreign import data CharNumMaybe :: Symbol -> Pattern
foreign import data CharNumPositive :: Symbol -> Pattern
foreign import data CharStar :: Symbol -> Pattern

foreign import kind PList
foreign import data PNil :: PList
foreign import data PCons :: Pattern -> PList -> PList

data PProxy (pl :: PList) = PProxy

class Parse (regex :: Symbol) (plist :: PList) | regex -> plist

instance parseImpl ::
  ( ReverseSymbol regex regex' -- backward parsing for post-decoration markup
  , ParseReversed regex' plist
  ) => Parse regex plist

class ParseReversed (regex :: Symbol) (plist :: PList) | regex -> plist

instance parseReversedEmpty :: ParseReversed "" PNil
else instance parseReversedNonEmpty ::
  ( Symbol.Cons h t regex -- h t <- regex
  , ParseDispatch h t plist
  ) => ParseReversed regex plist

class ParseDispatch (h :: Symbol) (t :: Symbol) (pl :: PList) | h t -> pl

instance parseDispatchNumFixedRightToken ::
  ( ParseCharNumFixedInt t int t1
  , ParseSingleChar t1 char t2
  , ParseReversed t2 restPl
  ) => ParseDispatch "}" t (PCons (CharNumFixed char int) restPl)
else instance parseDispatchMaybeToken ::
  ( ParseSingleChar t char t1
  , ParseReversed t1 restPl
  ) => ParseDispatch "?" t (PCons (CharNumMaybe char) restPl)
else instance parseDispatchNumPositiveToken ::
  ( ParseSingleChar t char t1
  , ParseReversed t1 restPl
  ) => ParseDispatch "+" t (PCons (CharNumPositive char) restPl)
else instance parseDispatchStarToken ::
  ( ParseSingleChar t char t1
  , ParseReversed t1 restPl
  ) => ParseDispatch "*" t (PCons (CharStar char) restPl)
else instance parseDispatchNumFixedLeftToken ::
  Fail
  ( Text "incomplete CharNumFixedInt"
  ) => ParseDispatch "{" t p
else instance parseDispatchLitTokenOrAnyToken ::
  ( ParseReversed t restPl
  ) => ParseDispatch h t (PCons (CharNumFixed h "1") restPl)

class ParseCharNumFixedInt (i :: Symbol) (int :: Symbol) (rest :: Symbol) | i -> int rest

instance parseCharNumFixedIntFirstCharEmpty ::
  Fail
  ( Text "incomplete CharNumFixedInt"
  ) => ParseCharNumFixedInt "" int rest
else instance parseCharNumFixedIntFirstCharNonEmpty ::
  ( Symbol.Cons h t i -- h t <- i
  , ParseCharNumFixedIntImpl h t int rest
  ) => ParseCharNumFixedInt i int rest

class ParseCharNumFixedIntImpl (h :: Symbol) (t :: Symbol) (int :: Symbol) (rest :: Symbol) | h t -> int rest

instance parseCharNumFixedIntSecondCharEmpty ::
  Fail
  ( Text "incomplete CharNumFixedInt"
  ) => ParseCharNumFixedIntImpl h "" int rest
else instance parseCharNumFixedIntSecondCharNonEmpty ::
  ( Symbol.Cons t_h t_t t -- t_h t_t <- t
  , ParseCharNumFixedIntDispatch h t_h t_t int rest
  ) => ParseCharNumFixedIntImpl h t int rest

class ParseCharNumFixedIntDispatch (h :: Symbol) (t_h :: Symbol) (t_t :: Symbol) (int :: Symbol) (rest :: Symbol) | h t_h t_t -> int rest

instance parseCharNumFixedIntDispatchBaseCase ::
  ParseCharNumFixedIntDispatch h "{" t_t h t_t
else instance parseCharNumFixedIntDispatchInductionStep ::
  ( ParseCharNumFixedIntImpl t_h t_t restInt rest
  , Symbol.Append h restInt int
  ) => ParseCharNumFixedIntDispatch h t_h t_t int rest

class ParseSingleChar (i :: Symbol) (char :: Symbol) (rest :: Symbol) | i -> char rest

instance parseSingleCharEmpty ::
  Fail
  ( Text "ParseSingleChar error: EOL"
  ) => ParseSingleChar "" char rest
else instance parseSingleCharNumFixedLeftToken ::
  Fail
  ( Text "ParseSingleChar error: invalid token, NumFixedLeftToken"
  ) => ParseSingleChar "{" char rest
else instance parseSingleCharNumFixedRightToken ::
  Fail
  ( Text "ParseSingleChar error: invalid token, NumFixedRightToken"
  ) => ParseSingleChar "}" char rest
else instance parseSingleCharMaybeToken ::
  Fail
  ( Text "ParseSingleChar error: invalid token, MaybeToken"
  ) => ParseSingleChar "?" char rest
else instance parseSingleCharNumPositiveToken ::
  Fail
  ( Text "ParseSingleChar error: invalid token, NumPositiveToken"
  ) => ParseSingleChar "+" char rest
else instance parseSingleCharStarToken ::
  Fail
  ( Text "ParseSingleChar error: invalid token, StarToken"
  ) => ParseSingleChar "*" char rest
else instance parseSingleCharNonEmpty ::
  (Symbol.Cons char rest i -- char rest <- i
  ) => ParseSingleChar i char rest


parse :: forall i o. Parse i o => SProxy i -> PProxy o
parse _ = PProxy :: PProxy o

-- Test
-- parseExample1 :: PProxy (PCons (CharNumMaybe "d") (PCons (CharStar ".") (PCons (CharStar "c") (PCons (CharNumFixed "b" "3") (PCons (CharNumFixed "a" "1") PNil)))))
-- parseExample1 = parse (SProxy :: SProxy "ab{3}c*.*d?")

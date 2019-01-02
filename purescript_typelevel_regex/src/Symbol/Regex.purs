module Symbol.Regex where

import Num.Digit as Digit
import Num.Int as Int
import Prim.TypeError (class Fail, Text)
import Symbol.Utils (class ReverseSymbol)
import Type.Data.Boolean as Bool
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol

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
  ( Digit.IsDigit h
  ) => ParseCharNumFixedIntDispatch h "{" t_t h t_t
else instance parseCharNumFixedIntDispatchInductionStep ::
  ( ParseCharNumFixedIntImpl t_h t_t restInt rest
  , Digit.IsDigit h
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
-- parseExample1 :: PProxy (PCons (CharNumMaybe "d") (PCons (CharStar ".") (PCons (CharStar "c") (PCons (CharNumFixed "b" "321") (PCons (CharNumFixed "a" "1") PNil)))))
-- parseExample1 = parse (SProxy :: SProxy "ab{123}c*.*d?")

class Recognize (pl :: PList) (str :: Symbol) (b :: Bool.Boolean) | pl str -> b

instance recoginzeImpl ::
  ( ReverseSymbol str str'
  , RecognizeReversed pl str' b
  ) => Recognize pl str b

class RecognizeReversed (pl :: PList) (str :: Symbol) (b :: Bool.Boolean) | pl str -> b

instance recognizeReversedBaseCase1 ::
  RecognizeReversed PNil "" Bool.True
else instance recognizeReversedBaseCase2 ::
  RecognizeReversed PNil str Bool.False
else instance recognizeReversedInductionStep ::
  ( RecognizeReversedDispatch p_h p_t str b
  ) => RecognizeReversed (PCons p_h p_t) str b

class RecognizeReversedDispatch (p_h :: Pattern) (p_t :: PList) (s :: Symbol) (b :: Bool.Boolean) | p_h p_t s -> b

instance recognizeReversedDispatchCharNumFixed ::
  ( RecognizeCharNumFixed char int p_t s b
  ) => RecognizeReversedDispatch (CharNumFixed char int) p_t s b
else instance recognizeReversedDispatchCharNumMaybe ::
  ( RecognizeCharNumMaybe char p_t s b
  ) => RecognizeReversedDispatch (CharNumMaybe char) p_t s b
else instance recognizeReversedDispatchCharNumPositive ::
  ( RecognizeCharNumPositive char p_t s b
  ) => RecognizeReversedDispatch (CharNumPositive char) p_t s b
else instance recognizeReversedDispatchCharStar ::
  ( RecognizeCharStar char p_t s b
  ) => RecognizeReversedDispatch (CharStar char) p_t s b

class RecognizeCharNumFixed (char :: Symbol) (int_reversed :: Symbol) (p_t :: PList) (s :: Symbol) (b :: Bool.Boolean) | char int_reversed p_t s -> b

instance recognizeCharNumFixedImpl ::
  ( ReverseSymbol int_inversed int
  , Int.Normalize int int'
  , RecognizeCharNumFixedBaseCase char int' p_t s b
  ) => RecognizeCharNumFixed char int_inversed p_t s b

class RecognizeCharNumFixedBaseCase (char :: Symbol) (int :: Symbol) (p_t :: PList) (s :: Symbol) (b :: Bool.Boolean) | char int p_t s -> b

instance recognizeCharNumFixedBaseCaseZero ::
  ( RecognizeReversed p_t s b
  ) => RecognizeCharNumFixedBaseCase char "0" p_t s b
else instance recognizeCharNumFixedBaseCasePositiveEmpty ::
  RecognizeCharNumFixedBaseCase char int p_t "" Bool.False
else instance recognizeCharNumFixedBaseCasePositiveNonEmpty ::
  ( Symbol.Cons s_h s_t s -- s_h s_t <- s
  , RecognizeCharNumFixedInductionStep char int p_t s_h s_t b
  ) =>  RecognizeCharNumFixedBaseCase char int p_t s b

class RecognizeCharNumFixedInductionStep (char :: Symbol) (int :: Symbol) (p_t :: PList) (s_h :: Symbol) (s_t :: Symbol) (b :: Bool.Boolean) | char int p_t s_h s_t -> b

instance recognizeCharNumFixedInductionStepAnyToken ::
  ( Int.Pred int int_pred
  , RecognizeCharNumFixedBaseCase "." int_pred p_t s_t b
  ) => RecognizeCharNumFixedInductionStep "." int p_t s_h s_t b
else instance recognizeCharNumFixedInductionStepLit ::
  ( Symbol.Equals c s_h isEqual
  , RecognizeCharNumFixedInductionStepLit isEqual c int p_t s_h s_t b
  ) => RecognizeCharNumFixedInductionStep c int p_t s_h s_t b

class RecognizeCharNumFixedInductionStepLit (isEqual :: Bool.Boolean) (char :: Symbol) (int :: Symbol) (p_t :: PList) (s_h :: Symbol) (s_t :: Symbol) (b :: Bool.Boolean) | isEqual char int p_t s_h s_t -> b

instance recognizeCharNumFixedInductionStepLitNotEqual ::
  RecognizeCharNumFixedInductionStepLit Bool.False char int p_t s_h s_t Bool.False
else instance recognizeCharNumFixedInductionStepLitEqual ::
  ( Int.Pred int int_pred
  , RecognizeCharNumFixedBaseCase char int_pred p_t s_t b
  ) => RecognizeCharNumFixedInductionStepLit Bool.True char int p_t s_h s_t b

class RecognizeCharNumMaybe (char :: Symbol) (p_t :: PList) (s :: Symbol) (b :: Bool.Boolean) | char p_t s -> b

instance recoginzeCharNumMaybeImpl ::
  ( RecognizeReversed p_t s b1
  , RecognizeCharNumMaybeDispatch b1 char p_t s b
  ) => RecognizeCharNumMaybe char p_t s b

class RecognizeCharNumMaybeDispatch (b1 :: Bool.Boolean) (char :: Symbol) (p_t :: PList) (s :: Symbol) (b :: Bool.Boolean) | b1 char p_t s -> b

instance recognizeCharNumMaybeDispatchTrue ::
  RecognizeCharNumMaybeDispatch Bool.True char p_t s Bool.True
else instance recognizeCharNumMaybeDispatchFalseEmpty ::
  RecognizeCharNumMaybeDispatch Bool.False char p_t "" Bool.False
else instance recognizeCharNumMaybeDispatchFalseNonEmpty ::
  ( Symbol.Cons s_h s_t s -- s_h s_t <- s
  , RecognizeCharNumMaybeInductionStep char p_t s_h s_t b
  ) => RecognizeCharNumMaybeDispatch Bool.False char p_t s b

class RecognizeCharNumMaybeInductionStep (char :: Symbol) (p_t :: PList) (s_h :: Symbol) (s_t :: Symbol) (b :: Bool.Boolean) | char p_t s_h s_t -> b

instance recognizeCharNumMaybeInductionStepAnyToken ::
  ( RecognizeReversed p_t s_t b
  ) => RecognizeCharNumMaybeInductionStep "." p_t s_h s_t b
else instance recognizeCharNumMaybeInductionStepLit ::
  ( Symbol.Equals c s_h isEqual
  , RecognizeCharNumMaybeInductionStepLit isEqual char p_t s_h s_t b
  ) => RecognizeCharNumMaybeInductionStep c p_t s_h s_t b

class RecognizeCharNumMaybeInductionStepLit (isEqual :: Bool.Boolean) (char :: Symbol) (p_t :: PList) (s_h :: Symbol) (s_t :: Symbol) (b :: Bool.Boolean) | isEqual char p_t s_h s_t -> b

instance recognizeCharNumMaybeInductionStepLitFalse ::
  RecognizeCharNumMaybeInductionStepLit Bool.False char p_t s_h s_t Bool.False
else instance recognizeCharNumMaybeInductionStepLitTrue ::
  ( RecognizeReversed p_t s_t b
  ) => RecognizeCharNumMaybeInductionStepLit Bool.True char p_t s_h s_t b

class RecognizeCharNumPositive (char :: Symbol) (p_t :: PList) (s :: Symbol) (b :: Bool.Boolean) | char p_t s -> b

class RecognizeCharNumPositiveInductionStep (char :: Symbol) (p_t :: PList) (s_h :: Symbol) (s_t :: Symbol) (b :: Bool.Boolean) | char p_t s_h s_t -> b

class RecognizeCharStar (char :: Symbol) (p_t :: PList) (s :: Symbol) (b :: Bool.Boolean) | char p_t s -> b

module Symbol.Regex where

import Prelude

-- kind Token
-- toToken :: Char -> Token
-- toToken '{' = NumFixedLeftToken
-- toToken '}' = NumFixedRightToken
-- toToken '?' = MaybeToken
-- toToken '+' = NumPositiveToken
-- toToken '.' = AnyToken
-- toToken '*' = StarToken
-- toToken c = LitToken c

foreign import kind Pattern
foreign import data CharNumFixed :: Symbol -> Symbol -> Pattern
foreign import data CharNumMaybe :: Symbol -> Pattern
foreign import data CharNumPositive :: Symbol -> Pattern
foreign import data CharStar :: Symbol -> Pattern

foreign import kind PList
foreign import data PNil :: PList
foreign import data PCons :: Pattern -> PList -> PList

class Parse (regex :: Symbol) (plist :: PList) | regex -> plist

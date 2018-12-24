module RuntimeParser where

import Prelude
import Data.Array (snoc) as A
import Data.Foldable (foldl)
import Data.String.CodeUnits (toCharArray, singleton) as String

data Fmt
  = Lit String
  | Var String

-- instance showFmt :: Show Fmt where
--   show = case _ of
--     Lit str -> "(Lit \"" <> str <> "\")"
--     Var str -> "(Var \"" <> str <> "\")"

type ErrorMsg = String

data Status
  -- = Clear -- no information carried from history, equivalent to (InLit "")
  = Invalid ErrorMsg
  | InLit String
  | InLitHitEscape String
  | InLitHitDollar String
  -- | HitLeftBracket -- equivalent to (InVar "")
  | InVar String
  -- | HitRightBracket -- equivalent to Clear

type State =
  { status :: Status
  , fl :: Array Fmt
  }

data Token
  = Regular Char
  | Escape
  | Dollar
  | LeftBracket
  | RightBracket
  | EndToken

tokenize :: Char -> Token
tokenize '\\' = Escape
tokenize '$' = Dollar
tokenize '{' = LeftBracket
tokenize '}' = RightBracket
tokenize c = Regular c

parse :: String -> Array Fmt
parse = _.fl
        <<< foldl reducer initState
        <<< (_ `A.snoc` EndToken)
        <<< map tokenize
        <<< String.toCharArray
  where
    initState :: State
    initState =
      { status : InLit ""
      , fl : []
      }

    reducer :: State -> Token -> State
    reducer state@({ status : InLit str, fl }) token = case token of
      Regular c -> state { status = InLit (str <> String.singleton c)}
      Dollar -> state { status = InLitHitDollar str }
      Escape -> state { status = InLitHitEscape str }
      LeftBracket -> state { status = InLit (str <> "{")}
      RightBracket -> state { status = InLit (str <> "}")}
      EndToken -> { status : InLit "", fl : fl `A.snoc` (Lit str)}

    reducer state@({ status : InLitHitEscape str }) token = case token of
      -- Regular _ -> state { status = Invalid "escape regular token"}
      Regular c -> state { status = InLit (str <> String.singleton c)}
      Dollar -> state { status = InLit (str <> "$") }
      Escape -> state { status = InLit (str <> "\\") }
      LeftBracket -> state { status = InLit (str <> "{")}
      RightBracket -> state { status = InLit (str <> "}")}
      EndToken -> state { status = Invalid "open escape token at the end"}

    reducer state@({ status : InLitHitDollar str, fl }) token = case token of
      Regular c -> state { status = InLit (str <> String.singleton c)}
      Dollar -> state { status = InLit (str <> "$") }
      Escape -> state { status = InLit (str <> "\\") }
      LeftBracket -> { status : InVar "", fl : fl `A.snoc` (Lit str)}
      RightBracket -> state { status = InLit (str <> "}")}
      EndToken -> { status : InLit "", fl : fl `A.snoc` (Lit str)}

    reducer state@({ status : InVar str, fl }) token = case token of
      -- TODO: restrict valid char set for field names
      -- TODO: multiple words split by spaces are not allowed
      Regular c -> state { status = InVar (str <> String.singleton c)}
      Dollar -> state { status = Invalid "$ in field name"}
      Escape -> state { status = Invalid "\\ in field name"}
      LeftBracket -> state { status = Invalid "{ in field name"}
      RightBracket -> { status : InLit "", fl : fl `A.snoc` (Var str)}
      EndToken -> state { status = Invalid "open { at the end" }

    reducer state@({ status : Invalid error }) _ = state

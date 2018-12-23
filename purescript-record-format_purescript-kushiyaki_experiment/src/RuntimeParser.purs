module RuntimeParser where

import Prelude
import Data.Array (snoc) as A
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.String.CodeUnits (toCharArray, singleton) as String

data Fmt
  = Lit String
  | Var String

type ErrorMsg = String

data Status
  = Clear -- no information carried from history
  | Invalid ErrorMsg
  | InLit String
  | InLitHitEscape String
  | HitDollar
  | HitLeftBracket
  | InVar String
  | HitRightBracket

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
    reducer ({ status : InLit str, fl }) EndToken
      = { status : Clear, fl : fl `A.snoc` (Lit str)}

    reducer ({ status : InLit str, fl }) Dollar
      = { status : HitDollar, fl : fl `A.snoc` (Lit str)}

    reducer state@({ status : InLit str }) Escape
      = state { status = InLitHitEscape str }

    reducer state@({ status : InLitHitEscape str }) token = case token of
      -- Regular _ -> state { status = Invalid "escape regular token"}
      Regular c -> state { status = InLit (str <> String.singleton c)}
      Dollar -> state { status = InLit (str <> "$") }
      Escape -> state { status = InLit (str <> "\\") }
      LeftBracket -> state { status = InLit (str <> "{")}
      RightBracket -> state { status = InLit (str <> "}")}
      EndToken -> state { status = Invalid "escape unfinished at the end"}

    reducer state@({ status : HitDollar }) LeftBracket
      = state { status = HitLeftBracket }

    -- TODO: finish the machine
    -- reducer state@({ status : HitDollar }) token
    --   = state { status = }

    reducer ({ status : InVar str, fl }) (Regular '}')
      = { status : HitRightBracket, fl : fl `A.snoc` (Var str)}

    reducer _ _ = { status : Clear, fl : [] }

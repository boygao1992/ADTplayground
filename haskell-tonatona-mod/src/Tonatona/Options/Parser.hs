module Tonatona.Options.Parser where

import RIO
import Options.Applicative (Parser, (<**>), helper, info, execParser)

class HasParser a where
  parser :: Parser a

parse :: HasParser a => IO a
parse = execParser $ info (parser <**> helper) mempty

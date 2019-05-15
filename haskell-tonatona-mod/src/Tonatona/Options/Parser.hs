module Tonatona.Options.Parser where

import RIO
import Options.Applicative (Parser, (<**>), helper, info, execParser)

class HasParser options where
  parser :: Parser options

parseOptions :: HasParser options => IO options
parseOptions = execParser $ info (parser <**> helper) mempty

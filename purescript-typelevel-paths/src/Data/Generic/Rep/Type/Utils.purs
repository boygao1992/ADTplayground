module Data.Generic.Rep.Type.Utils where

import Data.Generic.Rep (Argument, Constructor)

class GetConstructorName rep (name :: Symbol) | rep -> name
instance getConstructorNameImpl :: GetConstructorName (Constructor label arg) label

class GetArgument rep arg | rep -> arg
instance getArgumentImpl :: GetArgument (Constructor label (Argument arg)) arg

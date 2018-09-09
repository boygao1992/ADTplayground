module Log where

data Query next
  = AddMessage String next

data Output
  = OutMessage String

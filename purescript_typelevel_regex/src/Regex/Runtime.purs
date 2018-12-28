module Regex.Runtime where

import Prelude

case1StartEndToken = "^abc$" :: String --- "abc"
case2Number = "a{3}" :: String -- "aaa"
case3QuestionMark = "a?" :: String -- zero or one
case4Positive = "a+" :: String -- one or more
case5Any = "." :: String -- any char
case6CharStar = "a*" :: String -- zero or more
case7AnyStar = ".*" :: String -- ignore any number of consecutive chars, exhaustive search

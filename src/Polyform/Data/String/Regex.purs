module Polyform.Data.String.Regex where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.Regex (replace) as Regex
import Data.String.Regex.Flags (global) as Regex.Flags
import Data.String.Regex.Unsafe (unsafeRegex) as Regex.Unsafe

-- | Taken from: https://stackoverflow.com/questions/3446170/escape-string-for-use-in-javascript-regex
-- | function escapeRegExp(string) {
-- |   return string.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'); // $& means the whole matched string
-- | }
escape ∷ String → String
escape s = Regex.replace (Regex.Unsafe.unsafeRegex "[.*+?^${}()|[\\]\\\\]" Regex.Flags.global) "\\$&" s


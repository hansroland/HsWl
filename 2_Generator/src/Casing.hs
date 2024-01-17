{-# LANGUAGE OverloadedStrings #-}

module Casing
where

import Data.Char ( toUpper )
import qualified Data.Text as T
import Data.Text(Text)

-- | Conversions between several common identifier casing conventions:
--
-- Similar to https://github.com/tdammers/casing
--     but for Text data instead of strings
--
-- Here we convert only from snakeCase to camelCase or PascalCase
--
-- - @PascalCase@ - no spacing between words, first letter in word is
-- uppercase, all others are lowercase.
-- - @camelCase@ - like @PascalCase@, but the very first letter is lowercase.
-- - @snake_Case@ - underscores delimit words, case is unrestricted.


-- | An opaque type that represents a parsed identifier.
newtype Identifier a = Identifier { unIdentifier :: [a] }
    -- deriving (Show)

wordCase :: Text -> Text
wordCase txt =
    case T.uncons txt of
      Just (x,xs) -> T.cons (toUpper x) (T.toLower xs)
      Nothing       -> txt

-- | Convert from @snake_cased@ (either flavor)
fromSnake :: Text -> Identifier Text
fromSnake = Identifier . T.splitOn  "_"

-- | To @camelCase@
toCamel :: Identifier Text -> Text
toCamel (Identifier []) = ""
toCamel (Identifier (x:xs)) = T.concat $ T.toLower x:map wordCase xs

-- | To @PascalCase@
toPascal :: Identifier Text -> Text
toPascal = T.concat . map wordCase . unIdentifier

-- | From snake_Case to camelCase
snakeToCamel :: Text -> Text
snakeToCamel = toCamel . fromSnake

-- | From snake_Case to PascalCase
snakeToPascal :: Text -> Text
snakeToPascal = toPascal . fromSnake

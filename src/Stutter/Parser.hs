{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Stutter.Parser where

import Control.Applicative
import Control.Monad
import Text.Read (readMaybe)
import Data.Attoparsec.Text ((<?>))

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as T

import Stutter.Producer hiding (ProducerGroup)

type ProducerGroup = ProducerGroup_ ()

-------------------------------------------------------------------------------
-- Text
-------------------------------------------------------------------------------

parseText :: Atto.Parser T.Text
parseText = (<?> "text") $
    T.pack <$> Atto.many1 parseSimpleChar

parseSimpleChar :: Atto.Parser Char
parseSimpleChar = (<?> "simple char or escaped char") $
    -- A non-special char
    Atto.satisfy (`notElem` specialChars) <|>
    -- An escaped special char
    Atto.char '\\' *> Atto.anyChar

specialChars :: [Char]
specialChars =
    [
    -- Used for sum
      '|'
    -- Used for product
    , '#'
    -- Used for zip
    , '$'
    -- Used for Kleene plus
    , '+'
    -- Used for Kleene start
    , '*'
    -- Used for optional
    , '?'
    -- Used to delimit ranges
    , '[', ']'
    -- Used to scope groups
    , '(', ')'
    -- Used to replicate groups
    , '{', '}'
    -- Used for escaping
    , '\\'
    -- Used for files
    , '@'
    ]

parseGroup :: Atto.Parser ProducerGroup
parseGroup = (<?> "producer group") $
    (parseUnit' <**> parseSquasher' <*> parseGroup) <|>
    (PProduct <$> parseUnit' <*> parseGroup) <|>
    parseUnit'
  where
    parseUnit' = parseReplicatedUnit <|> parseUnit
    -- Default binary function to product (@#@)
    parseSquasher' = parseSquasher <|> pure PProduct

parseReplicatedUnit :: Atto.Parser ProducerGroup
parseReplicatedUnit = (<?> "replicated unary producer") $
    -- This the logic for the replication shouldn't be in the parser
      parseUnit <**> parseReplicator

type Squasher = ProducerGroup -> ProducerGroup -> ProducerGroup
type Replicator = ProducerGroup -> ProducerGroup

parseReplicator :: Atto.Parser Replicator
parseReplicator =
    parseKleenePlus <|>
    parseKleeneStar <|>
    parseOptional   <|>
    parseFoldApp

parseKleenePlus :: Atto.Parser Replicator
parseKleenePlus =
    Atto.char '+' *> pure (PRepeat)

parseKleeneStar :: Atto.Parser Replicator
parseKleeneStar =
    Atto.char '*' *> pure (PSum (PText T.empty) . PRepeat)

parseOptional :: Atto.Parser Replicator
parseOptional =
    Atto.char '?' *> pure (PSum (PText T.empty) )

parseFoldApp :: Atto.Parser Replicator
parseFoldApp =
    bracketed '{' '}'
      ( flip (,)
        <$> parseSquasher
        <*  Atto.char '|'
        <*> parseInt
    <|> (,PSum) <$> parseInt
      )
  <**>
    (pure (\(n, f) -> foldr1 f . replicate n))
  where
    parseInt :: Atto.Parser Int
    parseInt = (readMaybe <$> Atto.many1 Atto.digit) >>= \case
      Nothing -> mzero
      Just x -> return x

parseSquasher :: Atto.Parser Squasher
parseSquasher = Atto.anyChar >>= \case
  '|' -> return PSum
  '$' -> return PZip
  '#' -> return PProduct
  _ -> mzero

parseUnit :: Atto.Parser ProducerGroup
parseUnit = (<?> "unary producer") $
    PRanges <$> parseRanges <|>
    parseHandle             <|>
    PText <$> parseText     <|>
    bracketed '(' ')' parseGroup

bracketed :: Char -> Char -> Atto.Parser a -> Atto.Parser a
bracketed cl cr p = Atto.char cl *> p <* Atto.char cr

-- | Parse a Handle-like reference, preceded by an @\@@ sign. A single dash
-- (@-@) is interpreted as @stdin@, any other string is used as a file path.
parseHandle :: Atto.Parser ProducerGroup
parseHandle = (<?> "handle reference") $
    (flip fmap) parseFile $ \case
      "-" -> PStdin ()
      fp -> PFile fp

-------------------------------------------------------------------------------
-- File
-------------------------------------------------------------------------------

parseFile :: Atto.Parser FilePath
parseFile = (<?> "file reference") $
    T.unpack <$> (Atto.char '@' *> parseText)

-------------------------------------------------------------------------------
-- Ranges
-------------------------------------------------------------------------------

-- | Parse several ranges
--
-- Example:
--  @[a-zA-Z0-6]@
parseRanges :: Atto.Parser [Range]
parseRanges = (<?> "ranges") $
    Atto.char '['          *>
    Atto.many1 parseRange <*
    Atto.char ']'

-- | Parse a range of the form 'a-z' (int or char)
parseRange :: Atto.Parser Range
parseRange = (<?> "range") $
    parseIntRange <|> parseCharRange

-- | Parse a range in the format "<start>-<end>", consuming exactly 3
-- characters
parseIntRange :: Atto.Parser Range
parseIntRange = (<?> "int range") $
    IntRange <$>  ((,) <$> parseInt <* Atto.char '-' <*> parseInt)
  where
    parseInt :: Atto.Parser Int
    parseInt = (readMaybe . (:[]) <$> Atto.anyChar) >>= \case
      Nothing -> mzero
      Just x -> return x

-- | Parse a range in the format "<start>-<end>", consuming exactly 3
-- characters
parseCharRange :: Atto.Parser Range
parseCharRange = (<?> "char range") $
    CharRange <$> ((,) <$> Atto.anyChar <* Atto.char '-' <*> Atto.anyChar)

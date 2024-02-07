module Command where

import Text.Parsec hiding
  ( parse
  , choice
  , (<|>)
  , sepBy
  , sepBy1
  , many
  , many1
  )
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

import Item
import Direction

(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (try prsr1) prsr2

choice :: [Parser a] -> Parser a
choice = P.choice . map try

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy body sep = P.sepBy1 body (P.try sep)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 body sep = P.sepBy1 body (P.try sep)

many :: Parser a -> Parser [a]
many = P.many . try

many1 :: Parser a -> Parser [a]
many1 = P.many1 . try

parse :: Parser a -> String -> Either ParseError a
parse prsr = P.parse prsr ""

data Command
  = Inventory
  | Look
  | Drop [ItemName]
  | Take [ItemName]
  | Move Direction
  | Exit
  | Throw [ItemName]
  deriving (Eq, Show)

type Conjunction = [Command]

itemNameP :: Parser ItemName
itemNameP = choice
  [ pure Huntsman <* string "huntsman"
  , pure Jarate <* string "jarate"
  , pure MarketGardener <* string "marketgardener"
  , pure HuoLongHeater <* string "huolongheater"
  , pure Phlogistinator <* string "phlogistinator"
  , pure Sandvich <* string "sandvich"
  , pure Ambassador <* string "ambassador"
  , pure RocketLauncher <* string "rocketlauncher"
  , pure GoldenFryingPan <* string "goldenfryingpan"
  ]

nounPhrase_stub :: Parser [ItemName]
nounPhrase_stub = pure <$> itemNameP

nounPhrase :: Parser [ItemName]
nounPhrase = sepBy1 itemNameP (string "," *> string " " <|> string "")

inventoryP :: Parser Command
inventoryP = pure Inventory <* string "inventory"

takeP :: Parser Command
takeP = Take <$> (string "take " *> nounPhrase)

exitP :: Parser Command
exitP = pure Exit <* string "quit" <|> string "exit"

dropP :: Parser Command
dropP = Drop <$> (string "drop " *> nounPhrase)

lookP :: Parser Command
lookP = pure Look <* string "look"

directionP :: Parser Direction
directionP = choice
  [ pure N <* string "north"
  , pure S <* string "south"
  , pure E <* string "east"
  , pure W <* string "west"
  ]

moveP :: Parser Command
moveP = Move <$> directionP

throwP :: Parser Command
throwP = Throw <$> (string "throw " *> nounPhrase)

commandP :: Parser Command
commandP = choice
  [ inventoryP
  , takeP
  , exitP
  , dropP
  , lookP
  , moveP
  , throwP]

conjunctionP :: Parser Conjunction
conjunctionP = commandP `sepBy1` string " and " <* eof

parseInput :: String -> Maybe Conjunction
parseInput input = 
    case parse conjunctionP input of
    Left _ -> Nothing
    Right conjunction -> Just conjunction

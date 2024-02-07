module Item where

import qualified Data.Map as M

data ItemName
  = Huntsman
  | Jarate
  | MarketGardener
  | HuoLongHeater
  | Phlogistinator
  | Sandvich
  | Ambassador
  | RocketLauncher
  | GoldenFryingPan
  deriving (Eq, Ord)

instance Show ItemName where
    show Huntsman = "huntsman"
    show Jarate = "jarate"
    show MarketGardener = "marketgardener"
    show HuoLongHeater = "huolongheater"
    show Phlogistinator = "phlogistinator"
    show Sandvich = "sandvich"
    show Ambassador = "ambassador"
    show RocketLauncher = "rocketlauncher"
    show GoldenFryingPan = "goldenfryingpan"

type Universe = M.Map ItemName Item

data Item
    = Item { iname :: ItemName
           , weight :: Integer }
    deriving (Show, Eq)

mkUniverse :: [Item] -> Universe
mkUniverse [] = M.empty
mkUniverse items
    = M.fromList (map (\item -> (iname item, item)) items)

huntsman :: Item
huntsman = Item Huntsman 10

jarate :: Item
jarate = Item Jarate 1

marketgardener :: Item
marketgardener = Item MarketGardener 2

huolongheater :: Item
huolongheater = Item HuoLongHeater 50

phlogistinator :: Item
phlogistinator = Item Phlogistinator 60

sandvich :: Item
sandvich = Item Sandvich 3

ambassador :: Item
ambassador = Item Ambassador 4

rocketlauncher :: Item
rocketlauncher = Item RocketLauncher 70

goldenfryingpan :: Item
goldenfryingpan = Item GoldenFryingPan 80

univ :: Universe
univ = mkUniverse [huolongheater, huntsman, phlogistinator, marketgardener, jarate, rocketlauncher, goldenfryingpan, sandvich, ambassador]

itemNames :: [ItemName]
itemNames = map iname (M.elems univ)
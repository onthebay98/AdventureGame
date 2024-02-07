module Room where
import Item
import Direction
import Data.List

data RoomName
  = Hydro
  | Gullywash
  | Process
  | Sawmill
  | GoldRush
  deriving (Eq, Ord)

instance Show RoomName where
    show Hydro = "hydro"
    show Gullywash = "gullywash"
    show Process = "process"
    show Sawmill = "sawmill"
    show GoldRush = "gold rush"

type Exit = (Direction, RoomName)

data Room
    = Room { rname :: RoomName
           , desc :: String
           , exits :: [Exit]
           , objects :: [ItemName] }
    deriving (Show, Eq)

hydro :: Room
hydro = Room Hydro
               "Welcome to TC Hydro!"
               [(N, Sawmill), (E, Gullywash), (S, Process)]
               [Huntsman, HuoLongHeater]

gullywash :: Room
gullywash = Room Gullywash
               "Welcome to CP Gullywash!"
               [(W, Hydro)]
               [Sandvich, Ambassador]

process :: Room
process = Room Process
               "Welcome to CP Process!"
               [(N, Hydro)]
               [RocketLauncher]

livingRoom :: Room
livingRoom = Room Sawmill
               "Welcome to KOTH Sawmill!"
               [(N, GoldRush), (S, Hydro)]
               [Phlogistinator, MarketGardener]

goldRush :: Room
goldRush = Room GoldRush
               "Welcome to PL Goldrush!"
               [(S, Sawmill)]
               [Jarate]
               
roomNames :: [RoomName]
roomNames = map rname allRooms

addItem :: ItemName -> Room -> Room
addItem item room = room { rname = rname room
                         , desc = desc room 
                         , exits = exits room 
                         , objects = item : objects room }

removeItem :: ItemName -> Room -> Room
removeItem item room = room { rname = rname room
                         , desc = desc room 
                         , exits = exits room 
                         , objects = delete item (objects room) }

allRooms :: [Room]
allRooms = [hydro, livingRoom, gullywash, goldRush, process]

hasObjects :: Room -> Bool
hasObjects room = not $ null $ objects room
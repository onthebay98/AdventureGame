module Player where

import Item
import Room

import Data.List

data Player
    = Player { inventory :: [ItemName]
             , maxWeight :: Integer
             , location :: RoomName
             , looked :: Bool
             }
    deriving (Show, Eq)

addItem :: ItemName -> Player -> Player
addItem item plyr = plyr { inventory = item : inventory plyr }

removeItem :: ItemName -> Player -> Player
removeItem item plyr = plyr { inventory = delete item (inventory plyr) }

newLocation :: RoomName -> Player -> Player
newLocation rmnm plyr = plyr { location = rmnm, looked = False }

isCarryingAnything :: Player -> Bool
isCarryingAnything plyr = not (null (inventory plyr))

you :: Player
you = Player [] 100 Hydro False
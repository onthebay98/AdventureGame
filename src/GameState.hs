module GameState where
import Control.Exception
import qualified Data.Map as M

import Item
import Room
import Player
import Direction
import NPC
import System.Random

type GameMap = M.Map RoomName Room

data GameState
    = GameState {
        message :: Maybe String,
        gmap :: GameMap,
        universe :: Universe,
        player :: Player,
        npc :: NPC
    }
    deriving Show

nameAndRoom :: Room -> (RoomName, Room)
nameAndRoom room = (rname room, room)

mkMap :: [Room] -> GameMap
mkMap [] = M.empty
mkMap room
    = M.fromList (map nameAndRoom room)

gameMap :: GameMap
gameMap = mkMap allRooms

initialState :: GameState
initialState = GameState Nothing gameMap univ you sniper

data KeyError = KeyError
  deriving Show

instance Exception KeyError

getObjectUniv :: ItemName -> Universe -> Item
getObjectUniv iname u
  = case M.lookup iname u of
      Just obj -> obj
      Nothing -> throw KeyError

getObject :: ItemName -> GameState -> Item
getObject iname st = getObjectUniv iname (universe st)

getRoomMap :: RoomName -> GameMap -> Room
getRoomMap rname mp
  = case M.lookup rname mp of
      Just room -> room
      Nothing -> throw KeyError

getRoom :: RoomName -> GameState -> Room
getRoom name st = getRoomMap name (gmap st)

setRoomMap :: RoomName -> Room -> GameMap -> GameMap
setRoomMap roomname room = M.update (\_ -> Just room) roomname
-- the IDE suggested the above as opposed to the below
-- setRoomMap roomname room gmap = M.update (\_ -> Just room) roomname gmap

setMessage :: String -> GameState -> GameState
setMessage str gstate = gstate { message = if null str then Nothing else Just str }

currentInventory :: GameState -> [ItemName]
currentInventory gstate = inventory (player gstate)

currentRoom :: GameState -> Room
currentRoom gstate = getRoom (location (player gstate)) gstate

nearbyObjects :: GameState -> [ItemName]
nearbyObjects gstate = objects (currentRoom gstate)

getLeft :: Either String GameState -> String
getLeft (Left err) = err
getLeft _ = ""

takeItem :: ItemName -> GameState -> GameState
takeItem iname gstate = case
    (alreadyHaveTakeCheck iname gstate, inRoomTakeCheck iname gstate, weightCheck iname gstate) of
    (Right _, Right _, Right _) -> -- all checks passed
        let newPlayer = Player.addItem iname (player gstate)
            newRoom = Room.removeItem iname (currentRoom gstate)
        in setMessage ("You take the " <> show iname <> ".") gstate {
            gmap = setRoomMap (rname newRoom) newRoom (gmap gstate),
            player = newPlayer }
    _ ->  -- at least one failed
        let msg = filter (/= "") $ map getLeft [alreadyHaveTakeCheck iname gstate, inRoomTakeCheck iname gstate, weightCheck iname gstate]
        in setMessage (head msg) gstate

dropItem :: ItemName -> GameState -> GameState
dropItem iname gstate = case
    (anywhereDropCheck iname gstate, inRoomDropCheck iname gstate) of
    (Right _, Right _) -> -- all checks passed
        let newPlayer = Player.removeItem iname (player gstate) -- drop the item from player's inventory no matter what
            -- swapItems is True if NPC is in the room, player drops the desired item, and NPC has the winning item
            swapItems = checkSameRoom gstate && iname == npcItemWanted (npc gstate) && GoldenFryingPan `elem` npcInventory (npc gstate)
            newNPC =
                if swapItems
                then NPC.npcRemoveItem GoldenFryingPan (NPC.npcAddItem iname (npc gstate))
                else npc gstate -- leave npc as is
            newRoom =
                if swapItems
                then Room.addItem GoldenFryingPan (Room.removeItem iname (currentRoom gstate))
                else Room.addItem iname (currentRoom gstate) -- add the item to the room as normal
            npcMessage = "\nThe sniper sees the jarate and takes it.\nHe exclaims: \"Appreciate it, mate!\"\nAs a token of his appreciation, the sniper gives you his golden frying pan."
        in
            if swapItems
            then setMessage ("You drop the " <> show iname <> "." <> npcMessage) gstate {
            gmap = setRoomMap (rname newRoom) newRoom (gmap gstate),
            player = newPlayer,
            npc = newNPC }
            else setMessage ("You drop the " <> show iname <> ".") gstate {
            gmap = setRoomMap (rname newRoom) newRoom (gmap gstate),
            player = newPlayer,
            npc = newNPC }

    _ ->  -- at least one failed
        let msg = filter (/= "") $ map getLeft [anywhereDropCheck iname gstate, inRoomDropCheck iname gstate]
        in setMessage (head msg) gstate

inventoryWeight :: GameState -> Integer
inventoryWeight gstate = let
    items = map (`getObject` gstate) $ currentInventory gstate
    weights = map weight items
    in sum weights

type Error a = Either String a

alreadyHaveTakeCheck :: ItemName -> GameState -> Error GameState
alreadyHaveTakeCheck iname gstate =
    if iname `elem` currentInventory gstate
    then Left ("You are already carrying the " <> show iname)
    else Right gstate

inRoomTakeCheck :: ItemName -> GameState -> Error GameState
inRoomTakeCheck iname gstate =
    if iname `elem` nearbyObjects gstate
    then Right gstate
    else Left ("There is no " <> show iname <> " in this room.")

weightCheck :: ItemName -> GameState -> Error GameState
weightCheck iname gstate =
    if inventoryWeight gstate + weight (getObject iname gstate) > maxWeight (player gstate)
    then Left "That's too much weight for you to carry."
    else Right gstate

anywhereDropCheck :: ItemName -> GameState -> Error GameState
anywhereDropCheck iname gstate =
    case (alreadyHaveTakeCheck iname gstate, inRoomTakeCheck iname gstate) of
    (Right _, Left _) -> Left ("What do you mean, drop the " <> show iname <> "?")
    _ -> Right gstate

anywhereThrowCheck :: ItemName -> GameState -> Error GameState
anywhereThrowCheck iname gstate =
    case (alreadyHaveTakeCheck iname gstate, inRoomTakeCheck iname gstate) of
    (Right _, Left _) -> Left ("What do you mean, throw the " <> show iname <> "?")
    _ -> Right gstate

inRoomDropCheck :: ItemName -> GameState -> Error GameState
inRoomDropCheck iname gstate =
    case inRoomTakeCheck iname gstate of
    (Right _) -> Left ("You aren't carrying the " <> show iname)
    _ -> Right gstate

roomHasObjects :: GameState -> Bool
roomHasObjects gstate = hasObjects $ currentRoom gstate

destinationName :: Direction -> Room -> Maybe RoomName
destinationName dir room = lookup dir $ exits room

move :: Direction -> GameState -> GameState
move dir gstate = case
    lookup dir $ exits $ currentRoom gstate of
    Nothing ->
        setMessage "There is no exit in that direction." gstate
    Just roomName ->
        setMessage ("You go " <> show dir <> ".") gstate {
            player = newLocation roomName $ player gstate
        }

haveWonGame :: GameState -> Bool
haveWonGame gstate = location (player gstate) == Hydro && GoldenFryingPan `elem` currentInventory gstate

-- check if NPC is in same room as player
checkSameRoom :: GameState -> Bool
checkSameRoom gstate = location (player gstate) == npcLocation (npc gstate)

-- check if the player already looked
-- returns True if the player has already looked
checkLooked :: GameState -> Bool
checkLooked gstate = looked (player gstate)

choose2 :: [a] -> IO a
choose2 lst = do
  idx <- randomRIO (0, length lst - 1)
  return $ lst !! idx

randomRoomName :: GameState -> IO RoomName
randomRoomName gstate = do
    let xts = exits $ getRoom (npcLocation (npc gstate)) gstate
    (_, roomName) <- choose2 xts
    return roomName

moveNPC :: GameState -> IO GameState
moveNPC gstate = do
    newLoc <- randomRoomName gstate
    let newgstate = gstate { npc = (npc gstate) { npcLocation = newLoc } }
    return newgstate

throwItem :: ItemName -> GameState -> GameState
throwItem iname gstate = case
    (anywhereDropCheck iname gstate, inRoomDropCheck iname gstate, iname == npcItemWanted (npc gstate)) of
    (Right _, Right _, True) -> -- all checks passed, including item being npcItemWanted
        if checkSameRoom gstate
        then 
            let newPlayer = Player.removeItem iname (player gstate)
            in setMessage ("You throw the " <> show iname <> 
                 ".\nIt hits the sniper!\nHe's super pissed, and he says, 'I'm going to kill you, you better run!'")
                gstate { player = newPlayer,
                         npc = hitNPC (npc gstate) }
        else setMessage ("There's no one to throw the " <> show iname <> " at, so you don't.") gstate
    (Right _, Right _, False) -> -- the item attempted to be thrown is not npcItemWanted
        setMessage ("You can't throw the " <> show iname <> ".") gstate
    _ ->  -- at least one failed
        let msg = filter (/= "") $ map getLeft [anywhereThrowCheck iname gstate, inRoomDropCheck iname gstate]
        in setMessage (head msg) gstate
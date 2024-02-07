module NPC where

import Item
import Room
import Data.List

data NPC
    = NPC { npcInventory :: [ItemName]
          , npcLocation :: RoomName
          , npcItemWanted :: ItemName
          , hitByJarate :: Bool
          }
    deriving (Show, Eq)

-- sniper starts off in Hydro and wants the jarate, which he exchanges for the winning item
sniper :: NPC
sniper = NPC [GoldenFryingPan] Gullywash Jarate False

npcAddItem :: ItemName -> NPC -> NPC
npcAddItem item npc = npc { npcInventory = item : npcInventory npc }

npcRemoveItem :: ItemName -> NPC -> NPC
npcRemoveItem item npc = npc { npcInventory = delete item (npcInventory npc) }

hitNPC :: NPC -> NPC
hitNPC npc = npc { hitByJarate = True} 
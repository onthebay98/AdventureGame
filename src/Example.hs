module Example where

import Data.List
import System.Random

import Item
import Direction
import Room
import Player
import GameState
import NPC

choose :: [a] -> IO a
choose lst = do
  idx <- randomRIO (0, length lst - 1)
  return $ lst !! idx

exampleList :: IO a -> IO Int -> IO [a]
exampleList ioInt ioLen = do
  len <- ioLen
  sequence $ replicate len ioInt

class Example a where
  example :: IO a

instance Example Item where
  example = do 
    itemName <- choose itemNames
    itemWeight <- randomRIO (0, 100)
    return $ Item itemName itemWeight

instance Example Direction where
  example = choose [N, S, E, W]

exitExample :: IO Exit
exitExample = do
  randDirection <- example :: IO Direction
  randRoom <- choose roomNames
  return (randDirection, randRoom)

instance Example Room where
  example = do
    rname <- choose roomNames
    let desc = "You are in a randomly-generated room, which is the " <> show rname <> "."
    exits <- exampleList (exitExample :: IO Exit) (randomRIO (2, 4))
    objects <- exampleList (choose itemNames) (randomRIO (2, 5))
    return $ Room rname
                  desc
                  exits
                  objects

instance Example Player where
  example = do
    inventory <- exampleList (choose itemNames) (randomRIO (0, 10))
    maxWeight <- randomRIO (76, 84) -- bed (80) +/- beans (4) = (76, 84)
    location <- choose roomNames
    looked <- choose [True, False]
    return $ Player (nub inventory) maxWeight location looked

messageLookup :: [(Integer, String)]
messageLookup = [(0, "One possible message.")
                , (1, "Yet another possible message.")]

instance Example GameState where
  example = do
    rand <- choose [0,1,2]
    let message = lookup rand messageLookup 
    gmap <- exampleList (example :: IO Room) (randomRIO (2, 3))
    universe <- exampleList (example :: IO Item) (randomRIO (5, 10))
    player <- example :: IO Player
    let npc = sniper
    return $ GameState message (mkMap gmap) (mkUniverse universe) player npc
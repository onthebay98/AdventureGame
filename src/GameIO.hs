module GameIO where

import Control.Monad.State
import System.Exit
import System.IO

import GameState
import Player
import Room
import Command
import Item
import NPC

type GameIO a = StateT GameState IO a

prompt :: GameIO ()
prompt = lift $ putStr "-> " >> hFlush stdout

effectChange :: (GameState -> GameState) -> GameIO ()
effectChange f = do
  st <- get
  put (f st)

printMessage :: GameIO ()
printMessage = do
  st <- get
  case message st of
    Just msg -> do
      lift $ putStrLn msg
      put $ setMessage msg st
    Nothing -> return () 

printDescription :: GameIO ()
printDescription = do
  st <- get
  lift $ putStrLn $ desc $ getRoom (location $ player st) st

printObjects :: GameIO ()
printObjects = do
  st <- get
  let objs = objects $ getRoom (location $ player st) st
  if null objs
  then return ()
  else do
    lift $ putStrLn "You see the following objects:"
    printObjectsList objs

printObjectsList :: [ItemName] -> GameIO ()
printObjectsList [] = return ()
printObjectsList (obj: objs) = do
  lift $ putStrLn $ show obj
  printObjectsList objs

printExits :: GameIO ()
printExits = do
  st <- get
  let xts = exits $ getRoom (location $ player st) st
  if null xts
  then return ()
  else do
    lift $ putStrLn "There are exits in the following directions:"
    printExitsList xts

printExitsList :: [Exit] -> GameIO ()
printExitsList [] = return ()
printExitsList ((dir, _): xts) = do
  lift $ putStrLn $ show dir
  printExitsList xts

printInventory :: GameIO ()
printInventory = do
  st <- get
  let inv = inventory $ player st
  if null inv
  then lift $ putStrLn "You aren't carrying anything."
  else do
    lift $ putStrLn "You are carrying the following items:"
    printObjectsList inv

actionOverList :: (ItemName -> GameState -> GameState)
               -> [ItemName]
               -> GameIO ()
actionOverList _ [] = return ()
actionOverList action (item: items) = do
  st <- get
  let newSt = action item st
  put $ action item st
  printMessage
  actionOverList action items

finishGame :: GameIO ()
finishGame = do
  lift $ putStrLn "You successfully brought the golden frying pan into TC Hydro."
  lift $ putStrLn "Congrats! You win!"
  lift exitSuccess

loseGame :: GameIO ()
loseGame = do
  lift $ putStrLn "The sniper's caught up to you!"
  lift $ putStrLn "He exclaims, 'Everything above your neck's gonna be a fine red mist!'"
  lift $ putStrLn "You've been killed by the sniper. Game Over."
  lift exitSuccess

exit :: GameIO ()
exit = do
  lift $ putStrLn "Goodbye!"
  lift exitSuccess

checkGameOver :: GameIO ()
checkGameOver = do
  st <- get
  if haveWonGame st
  then finishGame
  else return ()

syntaxError :: GameIO ()
syntaxError = do
  lift $ putStrLn "I don't understand that."

opening :: GameIO ()
opening = do
  lift $ putStrLn "Welcome to Functional Adventure!"

performCommand :: Command -> GameIO ()
performCommand cmd = case cmd of
  Look -> do
    printDescription
    printObjects
    printExits
    checkNPCInRoom
  Move dir -> do
    st <- get
    put $ move dir st
    printMessage
  Inventory -> do
    printInventory
    checkNPCInRoom
  Take items -> do
    actionOverList takeItem items
    checkNPCInRoom
  Drop items -> do
    actionOverList dropItem items
    checkNPCInRoom
  Throw items -> do
    actionOverList throwItem items
  Exit -> do
    exit

performConjunction :: Conjunction -> GameIO ()
performConjunction [] = return ()
performConjunction (cmd: cmds) = do
  performCommand cmd
  performConjunction cmds

parseConjunction :: String -> GameIO ()
parseConjunction input = case parse conjunctionP input of
  Left _ -> syntaxError
  Right cmds -> performConjunction cmds

repl :: GameIO ()
repl = do
  st <- get
  lift $ putStr "-> "
  input <- lift getLine
  parseConjunction input
  checkGameOver

npcMessage :: GameIO ()
npcMessage = do
  st <- get
  if Jarate `elem` npcInventory (npc st) 
  then do lift $ putStrLn "Thanks for the jarate, mate!"
  else do
    if hitByJarate (npc st)
    then loseGame
    else do
      lift $ putStrLn "You see the sniper."
      if checkLooked st
      then return ()
      else do
        lift $ putStrLn "He says, 'Oy, I would love some jarate!'"
        jarateAndNPC
        put st {player = (player st) {looked = True}} -- updates player state to "looked = True"

checkNPCInRoom :: GameIO ()
checkNPCInRoom = do
  st <- get
  if checkSameRoom st -- if they are in the same room
  then npcMessage
  else do -- if they are not in the same room, then move the NPC
    updatedGameState <- lift $ moveNPC st
    put updatedGameState

jarateAndNPC :: GameIO ()
jarateAndNPC = do
  st <- get
  if checkSameRoom st && npcItemWanted (npc st) `elem` currentInventory st -- if same room and player has jarate
  then do lift $ putStrLn "You have the jarate. Do you drop it to trade for what the sniper has, or do you throw it at him?"
  else return ()
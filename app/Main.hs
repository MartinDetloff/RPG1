{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Brillo
import Brillo.Interface.IO.Interact (interactIO, Event (EventKey), SpecialKey (KeyUp, KeyDown, KeyLeft, KeyRight), KeyState (Down, Up), Modifiers (shift), Key (SpecialKey), MouseButton (LeftButton))
import Control.Exception (handle)
import Brillo.Data.ViewPort (ViewPort)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Brillo.Interface.IO.Simulate (simulateIO)
import Brillo.Interface.IO.Game (Key(MouseButton))
import System.Random (randomRIO)


data WorldState = WorldState {
    circlePos :: (Float, Float)
    , circleColor :: Color
    , circleRadius :: Float
    , keyHeldBool :: Bool
    , counter :: Int
    , enimies :: [(Float, Float)] 
    , window :: WindowDimensions
    , timeAcc :: Float
    , spriteFrames :: Picture
    , zombieSpriteIndex :: Int
    , zombiePic :: [Picture]
}

data WindowDimensions = Window {
    width :: Int
    ,height :: Int
}


handleInput :: Event -> WorldState ->  WorldState
handleInput (EventKey (SpecialKey KeyUp) Down _ _) (WorldState (x,y) clr radius b c enimies window timeAcc frames i z)  =
    -- do
        -- let newEnimies = 
        -- putStrLn "Up Key Pressed"
    (WorldState (x, y+25) clr radius b c enimies window timeAcc frames i z)

handleInput (EventKey (SpecialKey KeyDown) Down _ _) (WorldState (x,y) clr radius b c enimies window timeAcc frames i z) =
    --   do
    --     -- let newEnimies = 
    --     putStrLn "Down Key Pressed"
    (WorldState (x, y-25) clr radius b c enimies window timeAcc frames i z)

handleInput (EventKey (SpecialKey KeyLeft) Down _ _) (WorldState (x,y) clr radius b c enimies window timeAcc frames i z) =
    --   do
    --     -- let newEnimies = 
    --     putStrLn "Left Key Pressed"
    (WorldState (x-25, y) clr radius b c enimies window timeAcc frames i z)

handleInput (EventKey (SpecialKey KeyRight) Down _ _) (WorldState (x,y) clr radius b c enimies window timeAcc frames i z) =
    --   do
    --     -- let newEnimies = 
    --     putStrLn "Right Key Pressed"
    (WorldState (x+25, y) clr radius b c enimies window timeAcc frames i z)

-- handleInput (EventKey (MouseButton LeftButton) Down _ _) (WorldState (x,y) clr radius b c enimies window) = 
--     do  
--         putStrLn "Attacking anyone close"
--         newEnimies <- spawnNewEnemy (checkIfEnimiesClose enimies (x,y)) window 
--         (WorldState (x,y) clr radius b c newEnimies window)

handleInput _ (WorldState (x,y) clr radius b c enimies window timeAcc frames i z) =   
    (WorldState (x, y) clr radius b c enimies window timeAcc frames i z)


checkIfEnimiesClose :: [(Float, Float)] -> (Float, Float) -> [(Float, Float)]
checkIfEnimiesClose enimies (px,py) = [(ex, ey) | (ex, ey) <- enimies, sqrt (((ex - px)^2) + ((ey - py)^2)) > 50]


-- -- Function to spawn a new enemy
-- spawnNewEnemy :: [(Float, Float)] -> WindowDimensions-> IO [(Float, Float)]
-- spawnNewEnemy enimies (Window w h) = 
--     do
--         randomX <- randomRIO (fromIntegral (-w) / 2, fromIntegral w / 2)
--         randomY <- randomRIO (fromIntegral (-h) / 2, fromIntegral h / 2)
--         let newEnemy = (randomX, randomY)
--         return (newEnemy : enimies)


-- initialWorld :: WorldState
-- initialWorld = WorldState (0, 0) green 20  False 0 [(50,50), (100,100), (-50, -50), (-100, -100)] (Window 500 500) 0 frames


drawWorld :: WorldState ->  Picture
drawWorld (WorldState (x,y) clr radius _ _ enimies w timeAcc img i z) = 
    let currentZombieFrame = z !! (i `mod` length z)
    in (Pictures (Translate x y (Scale 0.5 0.5 img) : 
    [ Color red (Translate ex ey (Scale 0.5 0.5 currentZombieFrame)) | (ex,ey) <- enimies]))

-- updateGame :: ViewPort -> Float -> WorldState -> IO WorldState
-- updateGame _ _ world =  return (moveWorld world)

tempFun :: Float -> WorldState -> WorldState
tempFun steps (WorldState (x,y) clr radius b c enimies window timeAcc frames i z) = 
    
    let newTime = (steps + timeAcc)
        (newInx, resetTime) = if newTime >= 0.1
                              then ((i + 1), 0)
                              else (i, newTime) 

        newEnimies = moveEnimiesCloser enimies (x,y)
    in WorldState (x,y) clr radius b c newEnimies window resetTime frames newInx z


-- Function to move the enimies a little bit closer to the player 
moveEnimiesCloser :: [(Float, Float)] -> (Float, Float) -> [(Float, Float)] 
moveEnimiesCloser enimies (playerX, playerY) = 

    [(ex + dx, ey + dy) | (ex, ey) <- enimies,
     let dx = (playerX - ex) / 500, 
     let dy = (playerY - ey) / 500 ]


loadInZombieFrames :: IO [Picture]
loadInZombieFrames = sequence [loadBMP ("assets/zombie" ++ show x ++ ".bmp") | x <- [1..8]]


main :: IO ()
main = do
    mainCharacter <- loadBMP "assets/knight.bmp"
    z <- loadInZombieFrames
    let initialWorld =  WorldState (0, 0) green 20  False 0 [(50,50), (100,100), (-50, -50), (-100, -100)] (Window 500 500) 0 mainCharacter 0 z
    play 
        (InWindow "RPG" (500, 500) (100, 100) )
        black
        60
        initialWorld
        drawWorld
        handleInput
        (tempFun)


    -- interactIO
    --      (InWindow "RPG" (500, 500) (100, 100) )
    --      black
    --      initialWorld
    --      drawWorld
    --      handleInput
    --      (\_ -> return ())
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
}

data WindowDimensions = Window {
    width :: Int
    ,height :: Int
}


handleInput :: Event -> WorldState -> IO WorldState
handleInput (EventKey (SpecialKey KeyUp) Down _ _) (WorldState (x,y) clr radius b c enimies window) =
    do
        -- let newEnimies = 
        putStrLn "Up Key Pressed"
        return (WorldState (x, y+25) clr radius b c enimies window)

handleInput (EventKey (SpecialKey KeyDown) Down _ _) (WorldState (x,y) clr radius b c enimies window) =
      do
        -- let newEnimies = 
        putStrLn "Down Key Pressed"
        return (WorldState (x, y-25) clr radius b c enimies window)

handleInput (EventKey (SpecialKey KeyLeft) Down _ _) (WorldState (x,y) clr radius b c enimies window) =
      do
        -- let newEnimies = 
        putStrLn "Left Key Pressed"
        return (WorldState (x-25, y) clr radius b c enimies window)

handleInput (EventKey (SpecialKey KeyRight) Down _ _) (WorldState (x,y) clr radius b c enimies window) =
      do
        -- let newEnimies = 
        putStrLn "Right Key Pressed"
        return (WorldState (x+25, y) clr radius b c enimies window)

handleInput (EventKey (MouseButton LeftButton) Down _ _) (WorldState (x,y) clr radius b c enimies window) = 
    do  
        putStrLn "Attacking anyone close"
        newEnimies <- spawnNewEnemy (checkIfEnimiesClose enimies (x,y)) window 
        return (WorldState (x,y) clr radius b c newEnimies window)

handleInput _ (WorldState (x,y) clr radius b c enimies window) =  return (WorldState (x, y) clr radius b c enimies window)


checkIfEnimiesClose :: [(Float, Float)] -> (Float, Float) -> [(Float, Float)]
checkIfEnimiesClose enimies (px,py) = [(ex, ey) | (ex, ey) <- enimies, sqrt (((ex - px)^2) + ((ey - py)^2)) > 50]

spawnNewEnemy :: [(Float, Float)] -> WindowDimensions-> IO [(Float, Float)]
spawnNewEnemy enimies (Window w h) = 
    do
        randomX <- randomRIO (fromIntegral (-w) / 2, fromIntegral w / 2)
        randomY <- randomRIO (fromIntegral (-h) / 2, fromIntegral h / 2)
        let newEnemy = (randomX, randomY)
        return (newEnemy : enimies)


initialWorld :: WorldState
initialWorld = WorldState (0, 0) green 20  False 0 [(50,50), (100,100), (-50, -50), (-100, -100)] (Window 500 500)

drawWorld :: WorldState -> IO Picture
drawWorld (WorldState (x,y) clr radius _ _ enimies w) =
    return (Pictures (Translate x y (Color clr (Circle radius)) : 
    [ Color red (Translate ex ey (Circle 20)) | (ex,ey) <- enimies]))

-- updateGame :: ViewPort -> Float -> WorldState -> IO WorldState
-- updateGame _ _ world =  return (moveWorld world)


main :: IO ()
main = interactIO
         (InWindow "RPG" (500, 500) (100, 100) )
         black
         initialWorld
         drawWorld
         handleInput
         (\_ -> return ())
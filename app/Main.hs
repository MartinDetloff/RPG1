{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Brillo
import Brillo.Interface.IO.Interact (interactIO, Event (EventKey), SpecialKey (KeyUp, KeyDown, KeyLeft, KeyRight, KeySpace), KeyState (Down, Up), Modifiers (shift), Key (SpecialKey), MouseButton (LeftButton))
import Control.Exception (handle)
import Brillo.Data.ViewPort (ViewPort)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Brillo.Interface.IO.Simulate (simulateIO)
import Brillo.Interface.IO.Game (Key(MouseButton))
import System.Random (randomRIO)
import Brillo.Geometry.Angle (radToDeg)
-- import Math

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
    , background :: Picture
    , keysHeld :: KeysHeld
    , directionFacing :: Direction
}

data WindowDimensions = Window {
    width :: Float
    ,height :: Float
}

data KeysHeld = K {
      isUpHeld :: Bool
    , isDownHeld :: Bool
    , isLeftHeld :: Bool
    , isRightHeld :: Bool
}

data Direction = Dir {
    north :: Bool
    , south :: Bool
    , east :: Bool
    , west :: Bool
}

data Weapons = W {
    closeAttack :: Bool
    ,rangedAttack :: Bool
}


handleInput :: Event -> WorldState ->  WorldState
-- case for if you press down the up key 
handleInput (EventKey (SpecialKey KeyUp) Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir) =

    if y + 25 < (h / 2)

    then
        WorldState
            (x, y+25)
            clr
            radius
            b
            c
            enimies
            (Window w h)
            timeAcc
            frames
            i
            z
            background
            k {isUpHeld = True}
            dir {north = True, south = False, east = False, west = False}

    else
        WorldState
            (x, y)
            clr
            radius
            b
            c
            enimies
            (Window w h)
            timeAcc
            frames
            i
            z
            background
            k
            dir {north = True, south = False, east = False, west = False}

-- Case for if you let go of the upkey
handleInput (EventKey (SpecialKey KeyUp) Up _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir)  =

    WorldState
        (x, y)
        clr
        radius
        b
        c
        enimies
        (Window w h)
        timeAcc
        frames
        i
        z
        background
        k {isUpHeld = False}
        dir

handleInput (EventKey (SpecialKey KeyDown) Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir) =

    if y - 25 > (- (1 * (h / 2)))
    then
        WorldState
            (x, y-25)
            clr
            radius
            b
            c
            enimies
            (Window w h)
            timeAcc
            frames
            i
            z
            background
            (k {isDownHeld = True})
            dir {north = False, south = True, east = False, west = False}

    else
        WorldState
            (x, y)
            clr
            radius
            b
            c
            enimies
            (Window w h)
            timeAcc
            frames
            i
            z
            background
            k
            dir {north = False, south = True, east = False, west = False}


-- Case for if you let go of the downKey
handleInput (EventKey (SpecialKey KeyDown) Up _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir)  =
    WorldState
        (x, y)
        clr
        radius
        b
        c
        enimies
        (Window w h)
        timeAcc
        frames
        i
        z
        background
        k {isDownHeld = False}
        dir

-- Case for if you press the left Key
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir) =

    if x - 25 > (- (1 * (w / 2)))
    then
        WorldState
            (x-25, y)
            clr
            radius
            b
            c
            enimies
            (Window w h)
            timeAcc
            frames
            i
            z
            background
            (k {isLeftHeld = True})
            dir {north = False, south = False, east = False, west = True}

    else
        WorldState
            (x, y)
            clr
            radius
            b
            c
            enimies
            (Window w h)
            timeAcc
            frames
            i
            z
            background
            k
            dir {north = False, south = False, east = False, west = True}

-- Case for if you let go of the leftKey
handleInput (EventKey (SpecialKey KeyLeft) Up _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir)  =
    WorldState
        (x, y)
        clr
        radius
        b
        c
        enimies
        (Window w h)
        timeAcc
        frames
        i
        z
        background
        k {isLeftHeld = False}
        dir

-- Case for if you press the rightKey
handleInput (EventKey (SpecialKey KeyRight) Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir) =

    if x + 25 < (w / 2)
    then
        WorldState
            (x+25, y)
            clr
            radius
            b
            c
            enimies
            (Window w h)
            timeAcc
            frames
            i
            z
            background
            (k {isRightHeld = True})
            dir {north = False, south = False, east = True, west = False}

    else
        WorldState
            (x, y)
            clr
            radius
            b
            c
            enimies
            (Window w h)
            timeAcc
            frames
            i
            z
            background
            k
            dir {north = False, south = False, east = True, west = False}


-- Case for if you let go of the rightKey
handleInput (EventKey (SpecialKey KeyRight) Up _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir)  =
    WorldState
        (x, y)
        clr
        radius
        b
        c
        enimies
        (Window w h)
        timeAcc
        frames
        i
        z
        background
        k
        {isRightHeld = False}
        dir

-- Case for if you let go of the rightKey
handleInput (EventKey (SpecialKey KeySpace) Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir)  =
    let newEnimies = checkIfEnimiesWereKilled (x,y) enimies dir 
    in WorldState
        (x, y)
        clr
        radius
        b
        c
        newEnimies
        (Window w h)
        timeAcc
        frames
        i
        z
        background
        k
        dir

handleInput _ (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background kh dir) =
    WorldState
        (x, y)
        clr
        radius
        b
        c
        enimies
        (Window w h)
        timeAcc
        frames
        i
        z
        background
        kh
        dir


checkIfEnimiesClose :: [(Float, Float)] -> (Float, Float) -> [(Float, Float)]
checkIfEnimiesClose enimies (px,py) = [(ex, ey) | (ex, ey) <- enimies, sqrt (((ex - px)^2) + ((ey - py)^2)) > 50]



checkIfEnimiesWereKilled :: (Float, Float) -> [(Float, Float)] -> Direction -> [(Float, Float)]

checkIfEnimiesWereKilled (px, py) enimies dir = 
    [(ex, ey) | (ex, ey) <- enimies
    , checkIfLiveOrDead 
        (ex, ey) 
        (px, py) 
        (px + findChangeInXBasedUponDir dir) 
        (py + findChangeInYBasedUponDir dir) 
        dir
    ]

findChangeInXBasedUponDir :: Direction -> Float
findChangeInXBasedUponDir (Dir north south east west) 
        | east = 50
        | west = -50
        | otherwise = 0

findChangeInYBasedUponDir :: Direction -> Float
findChangeInYBasedUponDir (Dir north south east west) 
        | north = 50
        | south = -50
        | otherwise = 0

-- function to return true if the zombie lives and false if the zombie dies 
checkIfLiveOrDead :: (Float, Float) -> (Float, Float) -> Float -> Float -> Direction -> Bool
checkIfLiveOrDead (ex, ey) (px, py) maxX maxY (Dir north south east west) 
        | north && (ey < maxY && ey > py) = False
        | south && (ey > maxY && ey < py) = False
        | west && (ex > maxX && ex < px) = False
        | east && (ex < maxX && ex > px) = False
        | otherwise = True

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
drawWorld (WorldState (x,y) clr radius _ _ enimies w timeAcc img i z b kh dir) =
    let currentZombieFrame = z !! (i `mod` length z)
    in (Pictures (b :
    Translate x y (Scale 0.75 0.75 img) :
    [ let angle = angle2Player (x,y) (ex, ey)
    in Color red (Translate ex ey (Rotate (radToDeg ((-1 * angle)) + 90) ( currentZombieFrame))) | (ex,ey) <- enimies
    ]))

-- updateGame :: ViewPort -> Float -> WorldState -> IO WorldState
-- updateGame _ _ world =  return (moveWorld world)

tempFun :: Float -> WorldState -> WorldState
tempFun steps (WorldState (x,y) clr radius b c enimies window timeAcc frames i z background kh dir) =


    let newTime = (steps + timeAcc)
        (newInx, resetTime) = if newTime >= 0.1
                              then ((i + 1), 0)
                              else (i, newTime)
        newCharacterPos = changePosBasedUponKeyHolds (x,y) kh window
        newEnimies = moveEnimiesCloser enimies (x,y)
    in WorldState newCharacterPos clr radius b c newEnimies window resetTime frames newInx z background kh dir

angle2Player :: (Float, Float) -> (Float, Float) -> Float
angle2Player (px, py) (zx, zy) = atan2 (py - zy) (px - zx)



changePosBasedUponKeyHolds :: (Float, Float) -> KeysHeld -> WindowDimensions -> (Float, Float)
changePosBasedUponKeyHolds (playerX, playerY) (K isUpHeld isDownHeld isLeftHeld isRightHeld) (Window height width)
    | isUpHeld =
        if playerY + 3 < (height / 2)
        then (playerX, playerY + 3)
        else (playerX, playerY)

    | isDownHeld =
        if playerY - 3 > (-1 * (height / 2))
        then (playerX, playerY - 3)
        else (playerX, playerY)

    | isRightHeld =
        if  playerX + 3 < (width / 2)--playerX - 25 > (-1 * (width / 2))
        then (playerX + 3, playerY)
        else (playerX, playerY)

    | isLeftHeld =
        if playerX - 3 > (-1 * (width / 2))
        then (playerX - 3, playerY)
        else (playerX, playerY)

    | otherwise = (playerX, playerY)

-- Function to move the enimies a little bit closer to the player 
moveEnimiesCloser :: [(Float, Float)] -> (Float, Float) -> [(Float, Float)]
moveEnimiesCloser enimies (playerX, playerY) =

    [if sqrt ((playerX - ex)^2   + (playerY - ey)^2) > 50 then (ex + dx, ey + dy) else (ex, ey)| (ex, ey) <- enimies,
     let dx = (playerX - ex) / 100,
     let dy = (playerY - ey) / 100]

-- checkZombieCloseNess :: (Float, Float) -> [(Float, Float)] -> Bool
-- checkZombieCloseNess (zombieX, zombieY) allZombies = and [ if sqrt ((playerX - ex)^2   + (playerY - ey)^2) > 50 then (ex + dx, ey + dy) else (ex, ey) | (ex, ey) <- allZombies, (ex - (zombieX)) + (ey - (zombieY)) /= 0]

loadInZombieFrames :: IO [Picture]
loadInZombieFrames = sequence [loadBMP ("assets/zombieFrame" ++ show x ++ ".bmp") | x <- [1..2]]


main :: IO ()
main = do
    mainCharacter <- loadBMP "assets/knight.bmp"
    z <- loadInZombieFrames
    spaceBack <- loadBMP "assets/spaceBack.bmp"
    let initialWorld =  WorldState (0, 0) green 20  False 0 [(50,50), (100,100), (-50, -50), (-100, -100)] (Window 500 500) 0 mainCharacter 0 z spaceBack (K False False False False) (Dir False False True False)
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
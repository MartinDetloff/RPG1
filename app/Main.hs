{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
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
    , spriteFrames :: [Picture]
    , zombieSpriteIndex :: Int
    , zombiePic :: [Picture]
    , background :: Picture
    , keysHeld :: KeysHeld
    , directionFacing :: Direction
    , healthPoints :: Health
    , playerSpriteIndex :: Int
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

data Health = HP{
    health :: Int
    ,heartPositions :: [(Float, Float)]
    ,heartPictures :: [Picture]
    ,heartIsThere :: [Bool]
    ,allheartPictures :: [Picture]
}


handleInput :: Event -> WorldState ->  WorldState
-- case for if you press down the up key 
handleInput (EventKey (SpecialKey KeyUp) Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir health psi) =

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
            health
            psi

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
            health
            psi

-- Case for if you let go of the upkey
handleInput (EventKey (SpecialKey KeyUp) Up _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir health psi)  =

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
        health
        psi

handleInput (EventKey (SpecialKey KeyDown) Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir health psi) =

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
            health
            psi

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
            health
            psi


-- Case for if you let go of the downKey
handleInput (EventKey (SpecialKey KeyDown) Up _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir health psi)  =
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
        health
        psi

-- Case for if you press the left Key
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir health psi) =

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
            health
            psi

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
            health
            psi

-- Case for if you let go of the leftKey
handleInput (EventKey (SpecialKey KeyLeft) Up _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir health psi)  =
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
        health
        psi

-- Case for if you press the rightKey
handleInput (EventKey (SpecialKey KeyRight) Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir health psi) =

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
            health
            psi

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
            health
            psi


-- Case for if you let go of the rightKey
handleInput (EventKey (SpecialKey KeyRight) Up _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir health psi)  =
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
        health
        psi

-- Case for if you let go of the rightKey
handleInput (EventKey (SpecialKey KeySpace) Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir health psi)  =
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
        health
        psi


handleInput _ (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background kh dir health psi) =
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
        health
        psi

-- function to calculate the HP lost 
calculateHPLost :: [(Float, Float)] -> (Float, Float) -> Int
calculateHPLost enimies (px,py) = sum [1 | (ex, ey) <- enimies, sqrt (((ex - px)^2) + ((ey - py)^2)) < 50]

-- function to check if the enemies were killed and return a new list of enemies 
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

-- helper to find the change in X 
findChangeInXBasedUponDir :: Direction -> Float
findChangeInXBasedUponDir (Dir _ _ east west)
        | east = 50
        | west = -50
        | otherwise = 0

-- helper to find the change in Y 
findChangeInYBasedUponDir :: Direction -> Float
findChangeInYBasedUponDir (Dir north south _ _)
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



-- initialWorld :: WorldState
-- initialWorld = WorldState (0, 0) green 20  False 0 [(50,50), (100,100), (-50, -50), (-100, -100)] (Window 500 500) 0 frames

-- left = 90
-- down = 0
-- right = 270
-- up = 180

drawWorld :: WorldState ->  Picture
drawWorld (WorldState (x,y) clr radius _ _ enimies w timeAcc playerFrames i z b kh dir (HP health hposs pics heartsThere allheartPictures) psi) =
    let currentZombieFrame = z !! (i `mod` length z)
        currentPlayerFrame = playerFrames !! (psi `mod` length playerFrames)
        angle2Rotate = getRotateBasedUponDir dir
        -- newHp = health - calculateHPLost enimies (x,y)

    in Pictures (b :
    Translate x y (Rotate angle2Rotate currentPlayerFrame) :
    [ let angle = angle2Player (x,y) (ex, ey)
    in Color red (Translate ex ey (Rotate (radToDeg ((-1 * angle)) + 90) (currentZombieFrame))) | (ex,ey) <- enimies
    ] ++ [ if (heartsThere !! indx) then (Translate (fst pos) (snd pos) (pics !! 1)) else Translate (fst pos) (snd pos) (pics !! 0) | (indx, pos) <- zip [0..] hposs] )


removeTheHeartsLost :: [Bool] -> Int -> [Bool]
removeTheHeartsLost heartsThere newHp = [ newHp > indx  | (indx, isThereAHeart) <- zip [0..] (heartsThere)]
-- updateGame :: ViewPort -> Float -> WorldState -> IO WorldState
-- updateGame _ _ world =  return (moveWorld world)

getRotateBasedUponDir :: Direction -> Float
getRotateBasedUponDir (Dir north south east west)
                | north = 180
                | south = 0
                | east = 270
                | west = 90
                | otherwise = 0


tempFun :: Float -> WorldState -> WorldState
tempFun steps (WorldState (x,y) clr radius b c enimies window timeAcc frames i z background kh dir (HP health hposs pics heartsThere allheartPictures) psi) =


    let newTime = (steps + timeAcc)
        (newInx, resetTime) = if newTime >= 0.5
                              then ((i + 1), 0)
                              else (i, newTime)
        newCharacterPos = changePosBasedUponKeyHolds (x,y) kh window
        newEnimies = moveEnimiesCloser enimies (x,y) 1.5
        newHp =
            if newTime >= 0.5
            then health - calculateHPLost enimies (x,y)
            else health

        newHeartsThere = removeTheHeartsLost heartsThere newHp
        newAllHeartPictures = updateAllHeartPics allheartPictures heartsThere pics


    in WorldState
        newCharacterPos
        clr
        radius
        b
        c
        newEnimies
        window
        resetTime
        frames
        newInx
        z
        background
        kh
        dir
        HP {health = newHp, heartPositions = hposs, heartPictures = pics, heartIsThere = newHeartsThere, allheartPictures = newAllHeartPictures}
        newInx


-- function for the angle to the player 
angle2Player :: (Float, Float) -> (Float, Float) -> Float
angle2Player (px, py) (zx, zy) = atan2 (py - zy) (px - zx)

updateAllHeartPics :: [Picture] -> [Bool] -> [Picture] -> [Picture]
updateAllHeartPics allheartPictures isHeartThere avalibleOptions = [ if x then avalibleOptions !! 1 else avalibleOptions !! 0 | x <- isHeartThere]



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
        if playerX - 3 > (- (1 * (width / 2)))
        then (playerX - 3, playerY)
        else (playerX, playerY)

    | otherwise = (playerX, playerY)

-- Function to move the enimies a little bit closer to the player 
moveEnimiesCloser :: [(Float, Float)] -> (Float, Float) -> Float -> [(Float, Float)]
moveEnimiesCloser enimies (playerX, playerY) speed =

    [if sqrt (dx^2   +  dy^2) > 50  -- && not (checkIfTooCloseToOtherEnimies enimies (ex, ey))
        then (ex + (dx/ sqrt (dx^2   +  dy^2)) * speed, ey + (dy/ sqrt (dx^2   +  dy^2)) * speed) -- normalized vectors times the speed
        else (ex, ey) -- dont do anything
        | (ex, ey) <- enimies,
     let dx = (playerX - ex),
     let dy = (playerY - ey)
     ]

-- checkIfTooCloseToOtherEnimies :: [(Float, Float)] -> (Float, Float) -> Bool
-- checkIfTooCloseToOtherEnimies enimies (currEX, currEY) = True && (and [if (currEX - ex)^2 + ( currEY - ey  )^2 < 100 then True else False | (ex, ey) <- enimies, not ((ex - currEX) + (ey - currEY) == 0)])

-- checkZombieCloseNess :: (Float, Float) -> [(Float, Float)] -> Bool
-- checkZombieCloseNess (zombieX, zombieY) allZombies = and [ if sqrt ((playerX - ex)^2   + (playerY - ey)^2) > 50 then (ex + dx, ey + dy) else (ex, ey) | (ex, ey) <- allZombies, (ex - (zombieX)) + (ey - (zombieY)) /= 0]

loadInZombieFrames :: IO [Picture]
loadInZombieFrames = sequence [loadBMP ("assets/zombieFrame" ++ show x ++ ".bmp") | x <- [1..2]]

loadInPlayerFrames :: IO [Picture]
loadInPlayerFrames = sequence [loadBMP ("assets/knightFrame" ++ show x ++ ".bmp") | x <- [1..2]]

-- Heart 1 = Empty , Heart 2 = Full
loadInHealthFrames :: IO [Picture]
loadInHealthFrames = sequence [loadBMP ("assets/Heart" ++ show x ++ ".bmp") | x <- [1..2]]


main :: IO ()
main = do
    mainCharacter <- loadInPlayerFrames
    z <- loadInZombieFrames
    spaceBack <- loadBMP "assets/spaceBack.bmp"
    heartPictures <- loadInHealthFrames
    let initialHeart = heartPictures !! 1
    let windowHeight = 1000
    let windowWidth = 1000

    let initialWorld =  WorldState
                    (0, 0)
                    green
                    20
                    False
                    0
                    [(500,500), (-500,-500), (-500, 500), (500, -500)]
                    (Window windowHeight windowWidth)
                    0
                    mainCharacter
                    0
                    z
                    spaceBack
                    (K False False False False)
                    (Dir False False True False)
                    (
                    HP
                        5
                        [(-100, -(windowHeight/2) + 25 ), (-50, -(windowHeight/2) + 25 ), (0, -(windowHeight/2) + 25), (50, -(windowHeight/2) + 25), (100, -(windowHeight/2) + 25)]
                        heartPictures
                        [True, True, True, True, True]
                        [initialHeart, initialHeart, initialHeart, initialHeart, initialHeart]
                    )
                    0
    play
        FullScreen
        black
        60
        initialWorld
        drawWorld
        handleInput
        tempFun


    -- interactIO
    --      (InWindow "RPG" (500, 500) (100, 100) )
    --      black
    --      initialWorld
    --      drawWorld
    --      handleInput
    --      (\_ -> return ())
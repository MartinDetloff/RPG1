{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

import Brillo
import Brillo.Interface.IO.Interact (interactIO, Event (EventKey), SpecialKey (KeyUp, KeyDown, KeyLeft, KeyRight, KeySpace), KeyState (Down, Up), Modifiers (shift), Key (SpecialKey, Char), MouseButton (LeftButton))
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
    , enimies :: [Zombie]
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
    , currentWeapons :: Weapons
    , listOfLevels :: [Levels]
    , levelIndex :: Int

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
    closeAttack :: Bool,
    rangedAttack :: Bool,
    numOfBullets :: Int,
    bullets :: [Bullet]
}

data Health = HP{
    health :: Int
    ,heartPositions :: [(Float, Float)]
    ,heartPictures :: [Picture]
    ,heartIsThere :: [Bool]
    ,allheartPictures :: [Picture]
}

data Bullet = B {
    velocity :: (Float, Float),
    position :: (Float, Float),
    damage :: Int
} deriving (Eq)

data Zombie = Z {
    zPos :: (Float, Float),
    isAttacking :: Bool,
    zAttackTimer :: Float,
    -- zombieAttackFrames :: [Picture],
    -- zombieMoveFrames :: [Picture],
    zombieHP :: Int
} deriving (Eq)

data Levels = L {
    levelEnemyAmmount :: Int,
    levelInterval :: Float,
    totalTimeSpawning :: Float,
    spawnT :: Float
}

handleInput :: Event -> WorldState ->  WorldState
-- case for if you press down the up key 
handleInput (EventKey (SpecialKey KeyUp) Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir health psi cw lvls levelIndex) =

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
            cw
            lvls
            levelIndex

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
            cw
            lvls
            levelIndex

-- Case for if you let go of the upkey
handleInput (EventKey (SpecialKey KeyUp) Up _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir health psi cw lvls levelIndex)  =

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
        cw
        lvls
        levelIndex

handleInput (EventKey (SpecialKey KeyDown) Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir health psi cw lvls levelIndex) =

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
            cw
            lvls
            levelIndex

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
            cw
            lvls
            levelIndex


-- Case for if you let go of the downKey
handleInput (EventKey (SpecialKey KeyDown) Up _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir health psi cw lvls levelIndex)  =
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
        cw
        lvls
        levelIndex

-- Case for if you press the left Key
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir health psi cw lvls levelIndex) =

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
            cw
            lvls
            levelIndex

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
            cw
            lvls
            levelIndex

-- Case for if you let go of the leftKey
handleInput (EventKey (SpecialKey KeyLeft) Up _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir health psi cw lvls levelIndex)  =
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
        cw
        lvls
        levelIndex

-- Case for if you press the rightKey
handleInput (EventKey (SpecialKey KeyRight) Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir health psi cw lvls levelIndex) =

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
            cw
            lvls
            levelIndex

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
            cw
            lvls
            levelIndex


-- Case for if you let go of the rightKey
handleInput (EventKey (SpecialKey KeyRight) Up _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir health psi cw lvls levelIndex)  =
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
        cw
        lvls
        levelIndex

-- Case for if you let go of the rightKey
handleInput (EventKey (SpecialKey KeySpace) Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir health psi (W closeAttack rangedAttack numOfBullets bullets) lvls levelIndex)  =

    let newBullets = if rangedAttack
                     then spawnNewBullets bullets (x,y) dir
                     else bullets

        newEnimies = if closeAttack
                     then checkIfEnimiesWereKilled (x,y) enimies dir
                     else enimies

        newNumOfBullets = if ((length newBullets) > length bullets)
                          then (length bullets) - 1
                          else length bullets

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
        W {closeAttack = closeAttack, rangedAttack = rangedAttack, numOfBullets = newNumOfBullets, bullets = newBullets }
        lvls
        levelIndex


handleInput (EventKey (Char '1') Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background kh dir health psi cw lvls levelIndex) =

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
        cw {rangedAttack = False, closeAttack = True}
        lvls
        levelIndex

handleInput (EventKey (Char '2') Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background kh dir health psi cw lvls levelIndex) =

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
        cw {rangedAttack = True, closeAttack = False}
        lvls
        levelIndex

handleInput _ (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background kh dir health psi cw lvls levelIndex) =
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
        cw
        lvls
        levelIndex


-- shootBullet :: Direction -> (Float, Float) -> [Bullet] -> 
-- function to calculate the HP lost 
getAttackingZombies :: [Zombie] -> (Float, Float) -> [Zombie]
getAttackingZombies zombies (px, py) = [z | z <- zombies, distance (zPos z) (px, py) < 50]

distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2- x1)^2 + (y2-y1)^2)

-- function to check if the enemies were killed and return a new list of enemies 
checkIfEnimiesWereKilled :: (Float, Float) -> [Zombie] -> Direction -> [Zombie]
checkIfEnimiesWereKilled (px, py) zombies dir =
    [z | z <- zombies,
    let (ex, ey) = zPos z
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


-- function to spawn in a new bullet
spawnNewBullets :: [Bullet] -> (Float, Float) -> Direction -> [Bullet]
spawnNewBullets bullets (x, y) direction =
    B {velocity = makeVelocityBasedUponDirection direction, position = (x,y), damage = 1} : bullets

-- function to get velocity based upon a direction
makeVelocityBasedUponDirection :: Direction -> (Float, Float)
makeVelocityBasedUponDirection (Dir north south east west)
                | north = (0, 10)
                | south = (0, -10)
                | east = (10, 0)
                | west = (-10, 0)

-- initialWorld :: WorldState
-- initialWorld = WorldState (0, 0) green 20  False 0 [(50,50), (100,100), (-50, -50), (-100, -100)] (Window 500 500) 0 frames

-- left = 90
-- down = 0
-- right = 270
-- up = 180

drawWorld :: WorldState ->  Picture
drawWorld (WorldState (x,y) clr radius _ _ enimies w timeAcc playerFrames i z b kh dir (HP health hposs pics heartsThere allheartPictures) psi (W closeAttack rangedAttack numOfBullets bullets) lvls levelIndex) =
    let currentZombieFrame = z !! (i `mod` length z)
        currentPlayerFrame = playerFrames !! (psi `mod` length playerFrames)
        angle2Rotate = getRotateBasedUponDir dir
        -- newHp = health - calculateHPLost enimies (x,y)

    in Pictures (Scale 1.5 1.5 b :
    Translate x y (Rotate angle2Rotate currentPlayerFrame) :
    [ let angle = angle2Player (x,y) (ex, ey)
    in Color red (Translate ex ey (Rotate (radToDeg ((-1 * angle)) + 90) (currentZombieFrame))) | z <- enimies, let (ex,ey) = zPos z
    ] ++ [ if (heartsThere !! indx) then (Translate (fst pos) (snd pos) (pics !! 1)) else Translate (fst pos) (snd pos) (pics !! 0) | (indx, pos) <- zip [0..] hposs] ++ drawBullets bullets ++ drawZombieHitBoxes enimies)



drawBullets :: [Bullet] -> [Picture]
drawBullets bullets =  [Color black (Translate x y (Circle 10)) | b <- bullets,
                          let (x,y) = position b]



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
tempFun steps (WorldState (x,y) clr radius b c enimies window timeAcc frames i z background kh dir (HP health hposs pics heartsThere allheartPictures) psi cw lvls levelIndex) =


    let newTime = (steps + timeAcc)
        (newInx, resetTime) = if newTime >= 0.5
                              then ((i + 1), 0)
                              else (i, newTime)
        newCharacterPos = changePosBasedUponKeyHolds (x,y) kh window
        newEnimies = moveEnimiesCloser enimies (x,y) 1.5

        attackingZombies = getAttackingZombies enimies (x,y)

        updateZombies =
            [ if z `elem` attackingZombies
              then z {isAttacking = True, zAttackTimer = 0.3}
              else
                let newTimer = max 0 (zAttackTimer z - steps)
                in z {isAttacking = newTimer > 0, zAttackTimer = newTimer}
                | z <- newEnimies ]


        newHp =
            if newTime >= 0.5
            then health - length (getAttackingZombies enimies (x, y))
            else health

        level = lvls !! levelIndex
        newSpawnT = spawnT level + steps
        (newEnimiesP2, resetSpawnT) =
            if newSpawnT >= levelInterval level
            then (Z{zPos =(500, -500),isAttacking = False ,zAttackTimer= 0, zombieHP = 2} : newEnimies, 0)
            else (newEnimies, newSpawnT)

        updatedLevel = level {
            spawnT = resetSpawnT
        }

        newLevels = updateLevels lvls levelIndex updatedLevel 

        newBulletsPos = updateBulletsPos (bullets cw)

        checkForCollisions = checkForBulletCollisions (newBulletsPos) newEnimiesP2
        newBulletsList = fst checkForCollisions
        newZombiesList = snd checkForCollisions
        newHeartsThere = removeTheHeartsLost heartsThere newHp
        newAllHeartPictures = updateAllHeartPics allheartPictures heartsThere pics


    in WorldState
        newCharacterPos
        clr
        radius
        b
        c
        newZombiesList
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
        cw {bullets = newBulletsList}
        newLevels
        levelIndex


updateLevels ::  [Levels] -> Int -> Levels -> [Levels]
updateLevels levels indxOfOneToUpdate lvl1 = [if indx == indxOfOneToUpdate then lvl1 else lvl | (indx, lvl) <- zip [0..] levels]

updateBulletsPos :: [Bullet] -> [Bullet]
updateBulletsPos bullets = [b {position = (x + vx, y + vy)} | b <- bullets, let (vx, vy) = (velocity b), let (x, y) = (position b)]


checkForBulletCollisions :: [Bullet] -> [Zombie] -> ([Bullet], [Zombie])
checkForBulletCollisions bullets zombies =
    let hits = [(b, z) | b <- bullets, z <- zombies, distance (position b) (zPos z) < 30]
        newBullets = [b | (b, _) <- hits]
        newZombies = [if any (\(_, z1) -> z == z1) hits
                      then z {zombieHP = zombieHP z - 1}
                      else z | z <- zombies]
        survivingZombies = filter (\z -> zombieHP z > 0) newZombies
    in ([b | b <- bullets, b `notElem` newBullets], survivingZombies)

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
moveEnimiesCloser :: [Zombie] -> (Float, Float) -> Float -> [Zombie]
moveEnimiesCloser zombies (playerX, playerY) speed =

    [if sqrt (dx^2   +  dy^2) > 50  -- && not (checkIfTooCloseToOtherEnimies enimies (ex, ey))
        then z {zPos = (ex + (dx/ sqrt (dx^2   +  dy^2)) * speed, ey + (dy/ sqrt (dx^2   +  dy^2)) * speed)} -- normalized vectors times the speed
        else z  -- dont do anything
        | z <- zombies,
     let (ex, ey) = zPos z,
     let dx = (playerX - ex),
     let dy = (playerY - ey)
     ]

-- function to draw the zombie hitboxes 
drawZombieHitBoxes :: [Zombie] -> [Picture]
drawZombieHitBoxes zombies = [Translate x y (Color red (Circle 30)) | z <- zombies, let (x, y) = (zPos z)]

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

loadInZombieAttackingFrames :: IO [Picture]
loadInZombieAttackingFrames = sequence [loadBMP ("assets/zombieAttack" ++ show x ++ ".bmp") | x <- [1..2]]

loadInPlayerAttackingFrames :: IO [Picture]
loadInPlayerAttackingFrames = sequence [loadBMP ("assets/playerAttack" ++ show x ++ ".bmp") | x <- [1..2]]

main :: IO ()
main = do
    mainCharacter <- loadInPlayerFrames
    z <- loadInZombieFrames
    spaceBack <- loadBMP "assets/canyonBackground.bmp"
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
                    [
                    Z{zPos = (500,500),isAttacking = False ,zAttackTimer= 0, zombieHP = 2 },
                    Z{zPos = (-500,-500),isAttacking = False ,zAttackTimer= 0, zombieHP = 2},
                    Z{zPos = (-500, 500),isAttacking = False ,zAttackTimer= 0, zombieHP = 2},
                    Z{zPos =(500, -500),isAttacking = False ,zAttackTimer= 0, zombieHP = 2}
                     ]
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
                    W {closeAttack = False, rangedAttack = True, numOfBullets = 15, bullets = []}
                    [L {levelEnemyAmmount = 5, levelInterval = 5, totalTimeSpawning = 30, spawnT = 0}, L {levelEnemyAmmount = 10, levelInterval = 2.5, totalTimeSpawning = 35, spawnT = 0}]
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
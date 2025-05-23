{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

import Brillo
import Brillo.Interface.IO.Interact (interactIO, Event (EventKey), SpecialKey (KeyUp, KeyDown, KeyLeft, KeyRight, KeySpace, KeyEnter, KeyAltL, KeyBackspace, KeyCtrlL), KeyState (Down, Up), Modifiers (shift), Key (SpecialKey, Char), MouseButton (LeftButton))
import Control.Exception (handle)
import Brillo.Data.ViewPort (ViewPort)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Brillo.Interface.IO.Simulate (simulateIO)
import Brillo.Interface.IO.Game (Key(MouseButton), SpecialKey (..))
import System.Random (randomRIO)
import Brillo.Geometry.Angle (radToDeg)
import Data.Complex (magnitude)

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
    , backgrounds :: [Picture]
    , keysHeld :: KeysHeld
    , directionFacing :: Direction
    , healthPoints :: Health
    , playerSpriteIndex :: Int
    , currentWeapons :: Weapons
    , listOfLevels :: [Levels]
    , levelIndex :: Int
    , isGameOver :: Bool
    , gameOverPicture :: Picture
    , hotbar :: HotBar
    , zombieAttackFrames :: [Picture]
    , screenState :: ScreenState
    , knightInfo :: KnightInformation
    , bulletImage :: Picture
    , zombieBones :: [Bullet]
    , score :: Int
    , bonePic :: Picture
    , boneAngle :: Float
    , boss :: Boss
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
} deriving (Eq)

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
    direction :: Direction,
    damage :: Int
} deriving (Eq)

data Zombie = Z {
    zPos :: (Float, Float),
    isAttacking :: Bool,
    zAttackTimer :: Float,
    timeSinceShot :: Float,
    zombieAttackingFramesIndex :: Int,
    zombieHP :: Int,
    isBossZombie :: Bool,
    isRangedZombie :: Bool,
    isReadyToShoot :: Bool
} deriving (Eq)

data Levels = L {
    levelEnemyAmmount :: Int,
    levelInterval :: Float,
    totalTimeSpawning :: Float,
    spawnT :: Float
}

data HotBar = H {
    hotBarPictures :: [Picture],
    hotBarIndex :: Int
}

data ScreenState = Screen {
    mainMenu :: Bool,
    playingEndlessScreen :: Bool,
    playingStoryScreen :: Bool,
    endOfStoryScreen :: Bool,
    dialouge :: Bool,
    diag1Txt :: [String],
    diag2Txt :: [String],
    diag3Txt :: [String],
    diag4Txt :: [String],
    diag5Txt :: [String],
    diag6Txt :: [String],
    dialougeBackGround :: Picture
}

data Boss = Bozz {
    positionB :: (Float, Float),
    healthB :: Int,
    rangedAttackTimer :: Float,
    specialAttackTimer :: Float,
    bossPicture :: Picture,
    bossFireballSprite :: Picture,
    isBossAlive :: Bool,
    fireBalls :: [Bullet],
    isRdy2Shoot :: Bool,
    isRdy2Shoot2 :: Bool
}

data KnightInformation = Knight {
    knightAttackFrames :: [Picture],
    knightIsAttacking :: Bool,
    knightAttackTimer :: Float,
    knightAttackFrameIndex :: Int
}

spawnNewEnemies :: Int -> [Zombie]
spawnNewEnemies x = case x of
        0 -> []
        1 -> []
        2 -> [Z{zPos =(275, 250),isAttacking = False ,zAttackTimer= 0, zombieHP = 2, zombieAttackingFramesIndex = 0, isBossZombie = False, isRangedZombie = True, timeSinceShot  = 0.0, isReadyToShoot = False},
             Z{zPos =(400, 0),isAttacking = False ,zAttackTimer= 0, zombieHP = 2, zombieAttackingFramesIndex = 0, isBossZombie = False, isRangedZombie = False, timeSinceShot  = 0.0, isReadyToShoot = False},
             Z{zPos =(275, -250),isAttacking = False ,zAttackTimer= 0, zombieHP = 2, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = True, timeSinceShot  = 0.0, isReadyToShoot = False}
              ]

        3 -> [Z{zPos =(275, 250),isAttacking = False ,zAttackTimer= 0, zombieHP = 4, zombieAttackingFramesIndex = 0, isBossZombie = False, isRangedZombie = True, timeSinceShot  = 0.0, isReadyToShoot = False}
             ,Z{zPos =(400, 0),isAttacking = False ,zAttackTimer= 0, zombieHP = 15, zombieAttackingFramesIndex = 0, isBossZombie = True,  isRangedZombie = False, timeSinceShot  = 0.0, isReadyToShoot = False},
              Z{zPos =(275, -250),isAttacking = False ,zAttackTimer= 0, zombieHP = 4, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = True , timeSinceShot  = 0.0, isReadyToShoot = False},
              Z{zPos =(-275, -125),isAttacking = False ,zAttackTimer= 0, zombieHP = 4, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False, timeSinceShot  = 0.0, isReadyToShoot = False},
              Z{zPos =(-275, 125),isAttacking = False ,zAttackTimer= 0, zombieHP = 4, zombieAttackingFramesIndex = 0, isBossZombie = False, isRangedZombie = False, timeSinceShot  = 0.0, isReadyToShoot = False}
              ]

        4 -> [Z{zPos =(275, 250),isAttacking = False ,zAttackTimer= 0, zombieHP = 8, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = True, timeSinceShot  = 0.0, isReadyToShoot = False},
              Z{zPos =(400, 0),isAttacking = False ,zAttackTimer= 0, zombieHP = 20, zombieAttackingFramesIndex = 0, isBossZombie = True,  isRangedZombie = True, timeSinceShot  = 0.0, isReadyToShoot = False},
              Z{zPos =(275, -250),isAttacking = False ,zAttackTimer= 0, zombieHP = 8, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = True, timeSinceShot  = 0.0, isReadyToShoot = False},
              Z{zPos =(-275, -125),isAttacking = False ,zAttackTimer= 0, zombieHP = 8, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False, timeSinceShot  = 0.0, isReadyToShoot = False},
              Z{zPos =(-275, 125),isAttacking = False ,zAttackTimer= 0, zombieHP = 8, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False, timeSinceShot  = 0.0, isReadyToShoot = False},
              Z{zPos =(-275, 250),isAttacking = False ,zAttackTimer= 0, zombieHP = 8, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = True, timeSinceShot  = 0.0, isReadyToShoot = False},
              Z{zPos =(-400, 0),isAttacking = False ,zAttackTimer= 0, zombieHP = 20, zombieAttackingFramesIndex = 0, isBossZombie = True,  isRangedZombie = True, timeSinceShot  = 0.0, isReadyToShoot = False},
              Z{zPos =(-275, -250),isAttacking = False ,zAttackTimer= 0, zombieHP = 8, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = True, timeSinceShot  = 0.0, isReadyToShoot = False},
              Z{zPos =(275, -125),isAttacking = False ,zAttackTimer= 0, zombieHP = 8, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False, timeSinceShot  = 0.0, isReadyToShoot = False},
              Z{zPos =(275, 125),isAttacking = False ,zAttackTimer= 0, zombieHP = 8, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False, timeSinceShot  = 0.0, isReadyToShoot = False}]

        5 -> [Z{zPos =(0, 400),isAttacking = False ,zAttackTimer= 0, zombieHP = 25, zombieAttackingFramesIndex = 0, isBossZombie = True,  isRangedZombie = True, timeSinceShot  = 0.0, isReadyToShoot = False},
              Z{zPos =(400, 0),isAttacking = False ,zAttackTimer= 0, zombieHP = 25, zombieAttackingFramesIndex = 0, isBossZombie = True,  isRangedZombie = True, timeSinceShot  = 0.0, isReadyToShoot = False},
              Z{zPos =(-400, 0),isAttacking = False ,zAttackTimer= 0, zombieHP = 25, zombieAttackingFramesIndex = 0, isBossZombie = True,  isRangedZombie = True, timeSinceShot  = 0.0, isReadyToShoot = False},
              Z{zPos =(0, -400),isAttacking = False ,zAttackTimer= 0, zombieHP = 25, zombieAttackingFramesIndex = 0, isBossZombie = True,  isRangedZombie = True, timeSinceShot  = 0.0, isReadyToShoot = False}]
        6 -> [Z{zPos =(275, 250),isAttacking = False ,zAttackTimer= 0, zombieHP = 12, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False, timeSinceShot  = 0.0, isReadyToShoot = False}]
        7 -> []
        8 -> []



handleInput :: Event -> WorldState ->  WorldState

handleInput (EventKey (SpecialKey KeyEnter) Down _ _) worldState =
    let ss = dialouge (screenState worldState)
        newScreenState = if ss then (screenState worldState) {dialouge = False, playingStoryScreen = True} else screenState worldState
        newLevelIndex = levelIndex worldState + 1
        newZombies = spawnNewEnemies newLevelIndex
        newIsBossAlive = newLevelIndex == 6
    in worldState {screenState = newScreenState, levelIndex = newLevelIndex, enimies = newZombies, boss = (boss worldState) {isBossAlive = newIsBossAlive}}

-- you want to choose the endless mode
handleInput (EventKey (Char 'e') Down _ _) worldState =
                worldState {screenState = (screenState worldState) {mainMenu = False, playingEndlessScreen = True, playingStoryScreen = False, dialouge = False}}
-- you want to choose the storyMode
handleInput (EventKey (Char 's') Down _ _) worldState =
    worldState {screenState = (screenState worldState) {mainMenu = False, playingEndlessScreen = False, playingStoryScreen = True, dialouge = False}}

-- case where you press up key
handleInput (EventKey (SpecialKey KeyUp) Down _ _) worldState =

    if snd (circlePos worldState) + 25 < (height (window (worldState))  / 2)
    then
        worldState {circlePos = (fst (circlePos worldState) + 0, snd (circlePos worldState) + 25),
                    keysHeld = (keysHeld worldState) {isUpHeld = True, isDownHeld = False, isLeftHeld = False, isRightHeld = False},
                    directionFacing = (directionFacing worldState) {north = True, south = False, east = False, west = False}
                    }
    else
        worldState { directionFacing = (directionFacing worldState) {north = True, south = False, east = False, west = False}}

-- case where you press down key
handleInput (EventKey (SpecialKey KeyDown) Down _ _) worldState =

    if snd (circlePos worldState) - 25 > - (1 * (height (window (worldState)) / 2))
    then
        worldState {circlePos = (fst (circlePos worldState) + 0, snd (circlePos worldState) - 25),
                    keysHeld = (keysHeld worldState) {isDownHeld = True, isUpHeld = False, isLeftHeld = False, isRightHeld = False},
                    directionFacing = (directionFacing worldState) {north = False, south = True, east = False, west = False}
                    }
    else
        worldState { directionFacing = (directionFacing worldState) {north = False, south = True, east = False, west = False}}



-- case where you press left key
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) worldState =

    if fst (circlePos worldState) - 25 > - (1 * (width (window (worldState)) / 2))
    then
        worldState {circlePos = (fst (circlePos worldState) - 25, snd (circlePos worldState)),
                    keysHeld = (keysHeld worldState) {isDownHeld = False, isUpHeld = False, isLeftHeld = True, isRightHeld = False},
                    directionFacing = (directionFacing worldState) {north = False, south = False, east = False, west = True}
                    }
    else
        worldState { directionFacing = (directionFacing worldState) {north = False, south = False, east = False, west = True}}


-- case where you press right key
handleInput (EventKey (SpecialKey KeyRight) Down _ _) worldState =

    if fst (circlePos worldState) + 25 < (width (window (worldState)) / 2)
    then
        worldState {circlePos = (fst (circlePos worldState) + 25, snd (circlePos worldState)),
                    keysHeld = (keysHeld worldState) {isDownHeld = False, isUpHeld = False, isLeftHeld = False, isRightHeld = True},
                    directionFacing = (directionFacing worldState) {north = False, south = False, east = True, west = False}
                    }
    else
        worldState { directionFacing = (directionFacing worldState) {north = False, south = False, east = True, west = False}}

-- Case for if you let go of the rightKey
handleInput (EventKey (SpecialKey KeySpace) Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z 
                                                      background k dir health psi (W closeAttack rangedAttack numOfBullets bullets) lvls levelIndex isGameOver 
                                                      gameOverPic hb zaf screenState knightInfo bulletImage zombieBones score bonePic boneAngle boss)  =

    let newBullets = if rangedAttack
                     then spawnNewBullets bullets (x,y) dir
                     else bullets

        newEnimies = if closeAttack
                     then checkIfEnimiesWereKilled (x,y) enimies dir
                     else enimies

        newNumOfBullets = if ((length newBullets) > length bullets)
                          then (length bullets) - 1
                          else length bullets

        newKnightInfo = if closeAttack
                        then knightInfo {knightIsAttacking = True, knightAttackTimer = 1.3}
                        else knightInfo

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
        isGameOver
        gameOverPic
        hb
        zaf
        screenState
        newKnightInfo
        bulletImage
        zombieBones
        score
        bonePic
        boneAngle
        boss



handleInput (EventKey (Char '1') Down _ _) worldState =
    worldState {currentWeapons = (currentWeapons worldState) {rangedAttack = False, closeAttack = True},
            hotbar = (hotbar worldState) {hotBarIndex = 0}}


handleInput (EventKey (Char '2') Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z 
                                           background kh dir health psi cw lvls levelIndex isGameOver gameOverPic hb zaf 
                                           screenState knightInfo bulletImage zombieBones score bonePic boneAngle boss) =

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
        isGameOver
        gameOverPic
        hb {hotBarIndex = 1}
        zaf
        screenState
        knightInfo
        bulletImage
        zombieBones
        score
        bonePic
        boneAngle
        boss

handleInput _ (WorldState (x,y) clr radius b c enimies 
              (Window w h) timeAcc frames i z background kh dir health psi cw lvls levelIndex isGameOver gameOverPic 
              hb zaf screenState knightInfo bulletImage zombieBones score bonePic boneAngle boss) =
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
        isGameOver
        gameOverPic
        hb
        zaf
        screenState
        knightInfo
        bulletImage
        zombieBones
        score
        bonePic
        boneAngle
        boss


-- shootBullet :: Direction -> (Float, Float) -> [Bullet] -> 
-- function to calculate the HP lost 
getAttackingZombies :: [Zombie] -> (Float, Float) -> [Zombie]
getAttackingZombies zombies (px, py) = [z | z <- zombies, distance (zPos z) (px, py) < 50, not (isRangedZombie z)]

-- function to get the attacking ranged zombie 
getAttackingZombies2 :: [Zombie] -> (Float, Float) -> [Zombie]
getAttackingZombies2 zombies (px, py) = [z | z <- zombies, distance (zPos z) (px, py) < 600, isRangedZombie z]

--function to calculate the distance between 2 points
distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2- x1)^2 + (y2-y1)^2)

-- function to check if the enemies were killed and return a new list of enemies 
checkIfEnimiesWereKilled :: (Float, Float) -> [Zombie] -> Direction -> [Zombie]
checkIfEnimiesWereKilled (px, py) zombies dir =
    [if checkIfLiveOrDead (ex, ey) z
        (px, py)
        (px + findChangeInXBasedUponDir dir)
        (py + findChangeInYBasedUponDir dir)
        dir then z {zombieHP = zombieHP z - 1} else z| z <- zombies,
    let (ex, ey) = zPos z
    ]

-- helper to find the change in X 
findChangeInXBasedUponDir :: Direction -> Float
findChangeInXBasedUponDir (Dir _ _ east west)
        | east = 100
        | west = -100
        | otherwise = 0

-- helper to find the change in Y 
findChangeInYBasedUponDir :: Direction -> Float
findChangeInYBasedUponDir (Dir north south _ _)
        | north = 100
        | south = -100
        | otherwise = 0

-- function to return true if the zombie lives and false if the zombie dies 
checkIfLiveOrDead :: (Float, Float) -> Zombie -> (Float, Float) -> Float -> Float -> Direction -> Bool
checkIfLiveOrDead (ex, ey) z (px, py) maxX maxY (Dir north south east west)
        | north && (ey < maxY && ey > py) && (ex < px + 100)   = True
        | south && (ey > maxY && ey < py) && (ex < px + 100)   = True
        | west && (ex > maxX && ex < px) && (ey < py + 100 )    = True
        | east && (ex < maxX && ex > px) && (ey < py + 100 )    = True
        | otherwise = False

-- function to spawn in a new bullet
spawnNewBullets :: [Bullet] -> (Float, Float) -> Direction -> [Bullet]
spawnNewBullets bullets (x, y) direction =
    B {velocity = makeVelocityBasedUponDirection direction, position = (x,y), damage = 1, direction = direction} : bullets


-- function to spawn in new bones
spawnNewBones :: [Bullet] -> (Float, Float) -> [Zombie] -> [Bullet]
spawnNewBones bullets (px,py) zombies = 
    [B {velocity = (10 * dx / magnitude, 10 * dy / magnitude),
    position = (zx, zy),
    damage = 1,
    direction = Dir {north = True, south = False, east = False, west = False}} 
    | z <- zombies, let (zx,zy) = zPos z, let dx = px - zx, let dy = py - zy, let magnitude = sqrt (dx*dx + dy*dy), isReadyToShoot z] ++

    spawnEightBonesFromBoss bullets (filter isBossZombie zombies)


spawnNewFireBalls :: [Bullet] -> (Float, Float) -> Boss -> [Bullet]
spawnNewFireBalls bullets (px,py) boss 
    | not (isRdy2Shoot boss) = bullets
    | otherwise = 
    let (bx,by) = positionB boss
        dx = px - bx 
        dy = py - by 
        magnitude = sqrt (dx*dx + dy*dy)
    in [B {velocity = (5 * dx / magnitude, 5 * dy / magnitude),
    position = (bx, by),
    damage = 1,
    direction = Dir {north = True, south = False, east = False, west = False}}] ++ bullets 


spawnEightBonesFromBoss :: [Bullet] -> [Zombie] -> [Bullet]
spawnEightBonesFromBoss bullets zombies = bullets ++ concat [spawnEightBones bullets z  | z <-  zombies, let (zx, zy) = zPos z]

spawnEightBonesForBoss :: [Bullet] -> Boss -> [Bullet]
spawnEightBonesForBoss bullets boss 
    | (isRdy2Shoot2 boss) = 
    [B {velocity = (10, 0),
    position = positionB boss,
    damage = 1,
    direction = Dir {north = True, south = False, east = False, west = False}},

    B {velocity = (-10, 0),
    position = positionB boss,
    damage = 1,
    direction = Dir {north = True, south = False, east = False, west = False}},

    B {velocity = (0, 10),
    position = positionB boss,
    damage = 1,
    direction = Dir {north = True, south = False, east = False, west = False}},

    B {velocity = (0, -10),
    position = positionB boss,
    damage = 1,
    direction = Dir {north = True, south = False, east = False, west = False}},

    B {velocity = (7.071, 7.071),
    position = positionB boss,
    damage = 1,
    direction = Dir {north = True, south = False, east = False, west = False}},

    B {velocity = (7.071, -7.071),
    position = positionB boss,
    damage = 1,
    direction = Dir {north = True, south = False, east = False, west = False}},

    B {velocity = (-7.071, -7.071),
    position = positionB boss,
        damage = 1,
    direction = Dir {north = True, south = False, east = False, west = False}},

    B {velocity = (-7.071, 7.071),
    position = positionB boss,
    damage = 1,
    direction = Dir {north = True, south = False, east = False, west = False}}] ++ bullets

    | otherwise = bullets

spawnEightBones :: [Bullet] -> Zombie -> [Bullet]
spawnEightBones bullets zombie = [
    B {velocity = (10, 0),
    position = zPos zombie,
    damage = 1,
    direction = Dir {north = True, south = False, east = False, west = False}},

    B {velocity = (-10, 0),
    position = zPos zombie,
    damage = 1,
    direction = Dir {north = True, south = False, east = False, west = False}},

    B {velocity = (0, 10),
    position = zPos zombie,
    damage = 1,
    direction = Dir {north = True, south = False, east = False, west = False}},

    B {velocity = (0, -10),
    position = zPos zombie,
    damage = 1,
    direction = Dir {north = True, south = False, east = False, west = False}},

    B {velocity = (7.071, 7.071),
    position = zPos zombie,
    damage = 1,
    direction = Dir {north = True, south = False, east = False, west = False}},

    B {velocity = (7.071, -7.071),
    position = zPos zombie,
    damage = 1,
    direction = Dir {north = True, south = False, east = False, west = False}},

    B {velocity = (-7.071, -7.071),
    position = zPos zombie,
    damage = 1,
    direction = Dir {north = True, south = False, east = False, west = False}},

    B {velocity = (-7.071, 7.071),
    position = zPos zombie,
    damage = 1,
    direction = Dir {north = True, south = False, east = False, west = False}}]


-- function to get velocity based upon a direction
makeVelocityBasedUponDirection :: Direction -> (Float, Float)
makeVelocityBasedUponDirection (Dir north south east west)
                | north = (0, 10)
                | south = (0, -10)
                | east = (10, 0)
                | west = (-10, 0)

-- function to draw the world based upon a screen state
drawWorld :: WorldState ->  Picture
drawWorld worldState =
    case (mainMenu (screenState worldState), playingEndlessScreen (screenState worldState), dialouge (screenState worldState)) of
            (True, _, _) -> drawMainMenu worldState
            (False, True, _ ) -> drawPlayingEndlessScreen worldState
            (False, False, False) -> drawPlayingStoryScreen worldState
            (False, False, True) -> drawDiaglougeScreen worldState

-- function to draw the dialouge screens
drawDiaglougeScreen :: WorldState -> Picture
drawDiaglougeScreen ws = 
    case (levelIndex ws) of
        1 -> Pictures (dialougeBackGround (screenState ws) : drawMultiLineText (diag1Txt (screenState ws)) (-1700, 750))
        2 -> Pictures (dialougeBackGround (screenState ws) : drawMultiLineText (diag2Txt (screenState ws)) (-1000, 1000))
        3 -> Pictures (dialougeBackGround (screenState ws) : drawMultiLineText (diag3Txt (screenState ws)) (-1250, 1000))
        4 -> Pictures (dialougeBackGround (screenState ws) : drawMultiLineText (diag4Txt (screenState ws)) (-1500, 1250))
        5 -> Pictures (dialougeBackGround (screenState ws) : drawMultiLineText (diag5Txt (screenState ws)) (-1000, 1250))
        6 -> Pictures (dialougeBackGround (screenState ws) : drawMultiLineText (diag6Txt (screenState ws)) (-1000, 1250))

-- function to split texts into multiple lines
drawMultiLineText :: [String] -> (Float, Float)-> [Picture]
drawMultiLineText xs (ix, iy)= [ Scale 0.35 0.35 (Translate (ix) (iy - i * 250 ) (Color white (Text s))) | (s, i) <- zip xs [1..] ]

-- function to draw the main menu
drawMainMenu :: WorldState -> Picture
drawMainMenu (WorldState (x,y) clr radius _ _ enimies w timeAcc playerFrames i z b kh dir (HP health hposs pics heartsThere allheartPictures) 
             psi (W closeAttack rangedAttack numOfBullets bullets ) lvls levelIndex isGameOver gameOverPic (H hotBarPictures hotBarIndex) zaf 
             screenState knightInfo bulletImage zombieBones score bonePic boneAngle boss) =
    Pictures [Scale 2.5 1.5 (b !! 1)]

-- function to draw the endless gamemode
drawPlayingEndlessScreen :: WorldState -> Picture
drawPlayingEndlessScreen (WorldState (x,y) clr radius _ _ enimies w timeAcc playerFrames i z b kh dir (HP health hposs pics heartsThere allheartPictures) psi 
                         (W closeAttack rangedAttack numOfBullets bullets ) lvls levelIndex isGameOver gameOverPic (H hotBarPictures hotBarIndex) zaf screenState
                          knightInfo bulletImage zombieBones score bonePic boneAngle boss) =


    let currentZombieFrame = z !! (i `mod` length z)
        currentPlayerFrame = playerFrames !! (psi `mod` length playerFrames)
        angle2Rotate = getRotateBasedUponDir dir
        
        currentGameOver = if isGameOver then gameOverPic else Circle 0
       
        currentPlayerFrame2 = if knightIsAttacking knightInfo
                              then knightAttackFrames knightInfo !! knightAttackFrameIndex knightInfo
                              else currentPlayerFrame
        
        currentBackGround = if playingEndlessScreen screenState
                            then Scale 2.5 1.5 (b !! 0)
                            else Scale 1.75 1 (b !! (levelIndex))

    in Pictures (currentBackGround :
    
    Translate x y (Rotate angle2Rotate currentPlayerFrame2) :
    
    [ let angle = angle2Player (x,y) (ex, ey)
          currentZombieFrame2 = if attacking then zaf !! (zombieAttackingFramesIndex z `mod` length zaf) else currentZombieFrame
          in Color red (Translate ex ey (Rotate (radToDeg ((-1 * angle)) + 90) (if isBoss then Scale 2.0 2.0 currentZombieFrame2 else currentZombieFrame2))) |
          z <- enimies, let (ex,ey) = zPos z, let attacking = isAttacking z, let isBoss = isBossZombie z] ++

    [if heartsThere !! indx
     then  uncurry Translate pos (Scale 1.5 1.5 (pics !! 1))
     else uncurry Translate pos (Scale 1.5 1.5 (head pics))
     | (indx, pos) <- zip [0..] hposs] ++
    drawBullets bullets bulletImage ++
    -- drawZombieHitBoxes enimies ++
    [Scale 0.75 0.75 (Translate 0 (-(height w/2) + 25) (hotBarPictures !! hotBarIndex))] ++
    [Translate 0 0 currentGameOver] ++ 
    [Color black (Translate (-125) 300 (Scale 0.5 0.5 (Text (("Score " ++ show score)))))] ++
    [ drawBone boneAngle bonePic (bx,by) | bone <- zombieBones, let (bx, by) = position bone] ++ 
    drawZombieNames enimies black)


-- function to draw the story game mode
drawPlayingStoryScreen :: WorldState -> Picture
drawPlayingStoryScreen (WorldState (x,y) clr radius _ _ enimies w timeAcc playerFrames i z b kh dir (HP health hposs pics heartsThere allheartPictures) psi (W closeAttack rangedAttack numOfBullets bullets ) lvls levelIndex isGameOver gameOverPic (H hotBarPictures hotBarIndex) zaf screenState knightInfo bulletImage zombieBones score bonePic boneAngle boss) =

    let currentZombieFrame = z !! (i `mod` length z)
        currentPlayerFrame = playerFrames !! (psi `mod` length playerFrames)
        angle2Rotate = getRotateBasedUponDir dir
        
        currentGameOver = if isGameOver then gameOverPic else Circle 0
        
        currentPlayerFrame2 = if knightIsAttacking knightInfo
                              then knightAttackFrames knightInfo !! knightAttackFrameIndex knightInfo
                              else currentPlayerFrame
        
        currentBackGround = if playingEndlessScreen screenState
                            then Scale 2.5 1.5 (b !! 0)
                            else Scale 1.75 1 (b !! (levelIndex `mod` 7))
        
        currentBossSprite = if isBossAlive (boss)
                            then drawBoss boss
                            else Circle 0
        
        currentScore = if isBossAlive (boss) 
                       then [Circle 0]
                       else [Color white (Translate (-125) 300 (Scale 0.5 0.5 (Text (("Score " ++ show score)))))]

        

    in Pictures (currentBackGround :
    
    Translate x y (Rotate angle2Rotate currentPlayerFrame2) :
    
    [ let angle = angle2Player (x,y) (ex, ey)
          currentZombieFrame2 = if attacking then zaf !! (zombieAttackingFramesIndex z `mod` length zaf) else currentZombieFrame
          in Color red (Translate ex ey (Rotate (radToDeg ((-1 * angle)) + 90) (if isBoss then Scale 2.0 2.0 currentZombieFrame2 else currentZombieFrame2))) |
          z <- enimies, let (ex,ey) = zPos z, let attacking = isAttacking z, let isBoss = isBossZombie z] ++

    [if heartsThere !! indx then  uncurry Translate pos (Scale 1.5 1.5 (pics !! 1)) else uncurry Translate pos (Scale 1.5 1.5 (head pics)) | (indx, pos) <- zip [0..] hposs] ++
    drawBullets bullets bulletImage ++
    -- drawZombieHitBoxes enimies ++
    [Scale 0.75 0.75 (Translate 0 (-(height w/2) + 25) (hotBarPictures !! hotBarIndex))] ++
    [Translate 0 0 currentGameOver] ++
    [ drawBone boneAngle bonePic (bx,by) | bone <- zombieBones, let (bx, by) = position bone] ++
    (drawZombieNames enimies white) ++ 
    [currentBossSprite]++ 
    currentScore ++ 
    [drawFireBalls boneAngle (bossFireballSprite boss) (fbx, fby) | fb <- (fireBalls boss), let (fbx, fby) = position fb]) 

-- function to draw fire balls
drawFireBalls :: Float -> Picture -> (Float, Float) -> Picture
drawFireBalls angle ballPic (bx, by) = ((Translate bx by (Rotate angle ((Scale 0.05 0.05 ((ballPic)))))))

-- function to draw the boss
drawBoss :: Boss -> Picture
drawBoss b = Pictures [((Translate (fst(positionB (b)))  (snd(positionB (b))) (Scale 0.20 0.20 (bossPicture (b)))))]
-- : [Translate (fst(positionB (b))) (snd(positionB (b))) (Color red (Circle 100))])

-- funciton to draw a bone
drawBone :: Float -> Picture -> (Float, Float) -> Picture
drawBone angle bonePic (bx, by) = ((Translate bx by (Rotate angle ((Scale 0.05 0.05 ((bonePic)))))))

-- funciton to draw the zombies names 
drawZombieNames :: [Zombie] -> Color -> [Picture]
drawZombieNames zombies clr =  
    [Color clr (Translate (zx - 50) (zy + 50) (Scale 0.1 0.1 ((if isBoss then Text "Elite Zombie" else (if rangedZ then Text "Ranged Zombie" else Text "Close Zombie"))))) 
    | z <- zombies, let isBoss = isBossZombie z, let rangedZ = isRangedZombie z, let (zx, zy) = zPos z]

-- function to draw bullets
drawBullets :: [Bullet] -> Picture -> [Picture]
drawBullets bullets bulletImage =  [Color green (translate x y (Rotate (getRotateBasedUponDir (direction b) + 180) (bulletImage))) | b <- bullets,
                          let (x,y) = position b]


-- function to remove the hearts that were lost
removeTheHeartsLost :: [Bool] -> Int -> [Bool]
removeTheHeartsLost heartsThere newHp = [ newHp > indx  | (indx, isThereAHeart) <- zip [0..] (heartsThere)]

-- function to map directions to rotations
getRotateBasedUponDir :: Direction -> Float
getRotateBasedUponDir (Dir north south east west)
                | north = 180
                | south = 0
                | east = 270
                | west = 90
                | otherwise = 0

-- function to update the worldstate based upon the screen state
tempFun :: Float -> WorldState -> WorldState
tempFun steps world@(WorldState (x,y) clr radius b c enimies window timeAcc frames i z background kh dir 
                    (HP health hposs pics heartsThere allheartPictures) psi cw lvls levelIndex isGameOver 
                    gameOverPic hb zaf screenState knightInfo bulletImage zombieBones score bonePic boneAngle boss)

    | health <= 0 = world {isGameOver = True}
    | isGameOver = world
    | otherwise =

    case (mainMenu (screenState), playingEndlessScreen (screenState), dialouge (screenState)) of
        (False, True, _) ->
            updateWorldState4Endless steps world
        (True, False, _) ->
            (WorldState (x,y) clr radius b c enimies window timeAcc frames i z background kh dir (HP health hposs pics heartsThere allheartPictures) psi cw lvls levelIndex isGameOver gameOverPic hb zaf screenState knightInfo bulletImage zombieBones score bonePic boneAngle boss)
        (False, False, False) ->
            updateWorldState4Story steps world
        (False, False, True) ->
            updateWorldStateDiag steps world

-- function to keep the same world state for dialouge screens
updateWorldStateDiag :: Float -> WorldState -> WorldState
updateWorldStateDiag steps ws = ws

-- function to update the world state for the endless mode 
updateWorldState4Endless :: Float -> WorldState -> WorldState
updateWorldState4Endless steps (WorldState (x,y) clr radius b c enimies window timeAcc frames i z background kh dir (HP health hposs pics heartsThere allheartPictures) psi cw lvls levelIndex isGameOver gameOverPic hb zaf screenState knightInfo bulletImage zombieBones score bonePic boneAngle boss) =
    let newTime = (steps + timeAcc)
        (newInx, resetTime) = if newTime >= 0.5
                              then ((i + 1), 0)
                              else (i, newTime)
        newCharacterPos = changePosBasedUponKeyHolds (x,y) kh window
        newEnimies = moveEnimiesCloser enimies (x,y) 1.5

        attackingZombies = getAttackingZombies enimies (x,y)
        randgedAttackingZombies = getAttackingZombies2 enimies (x,y)

        (newZombieIndex, resetZombieTimer) = if newTime >= 0.25
                                             then (1, 0)
                                             else (0, newTime)
        updateZombies =
                    [ if z `elem` attackingZombies
                    then z {isAttacking = True, zAttackTimer = 0.5, zombieAttackingFramesIndex = zombieAttackingFramesIndex z + newZombieIndex}
                    else
                        let newTimer = max 0 (zAttackTimer z - steps)
                        in z {isAttacking = newTimer > 0, zAttackTimer = resetZombieTimer, zombieAttackingFramesIndex = 0}
                        | z <- newEnimies ]

        updateZombies2 =  [ if z `elem` randgedAttackingZombies
                            then (
                                if timeSinceShot z > 1.0
                                then z {timeSinceShot = 0.0, isReadyToShoot = True}
                                else let newTimeSince = max 0.0 (timeSinceShot z + steps)
                                     in  z {timeSinceShot = newTimeSince})

                            else z | z <- updateZombies]

        newZombieBones = spawnNewBones zombieBones (x,y) [z | z <- updateZombies2, isReadyToShoot z ]

        updateZombies3 = [z {isReadyToShoot = False} | z <- updateZombies2]


        (newKnightIndex, resetKnightTimer) = if newTime >= 1
                                             then (1, 0)
                                             else (0, newTime)
        updateKnightAttacking =
            if knightIsAttacking (knightInfo)
            then
                let newTimer = max 0.0 (knightAttackTimer knightInfo - steps)
                    stillAttacking = newTimer > 0
                    totalAttackDuration = 1.3
                    frameDuration = totalAttackDuration / fromIntegral (length (knightAttackFrames knightInfo))
                    elapsed = totalAttackDuration - newTimer

                    frame = floor (elapsed / frameDuration)
                    newIndex = if stillAttacking then frame else 0
                in knightInfo {knightIsAttacking = stillAttacking, knightAttackTimer = resetKnightTimer, knightAttackFrameIndex = newIndex }
            else
                knightInfo


        newHp =
                    if newTime >= 0.5
                    then health - length (getAttackingZombies enimies (x, y))
                    else health

        newGameOverState = newHp == 0


        level = lvls !! levelIndex
        newSpawnT = spawnT level + steps
        (newEnimiesP2, resetSpawnT) =
                    if newSpawnT >= levelInterval level
                    then (Z{zPos =(1000, -500),isAttacking = False ,zAttackTimer= 0, zombieHP = 2, zombieAttackingFramesIndex = 0, isBossZombie = False, isRangedZombie = True , timeSinceShot  = 0.0 , isReadyToShoot = False} : updateZombies2, 0)
                    else (updateZombies3, newSpawnT)

        updatedLevel = level {
                    spawnT = resetSpawnT
                }

        newLevels = updateLevels lvls levelIndex updatedLevel

        newBulletsPos = updateBulletsPos (bullets cw)
        newBonesPos = updateBulletsPos newZombieBones

        checkForCollisions = checkForBulletCollisions (newBulletsPos) newEnimiesP2
        (newBonesL, newHP2) = checkForBulletCollisions2 (newBonesPos) (x,y) newHp
        

        newfinalBones = (despawnOutOfBoundsBones (height window) (width window) newBonesL)
        newBulletsList = fst checkForCollisions
        newZombiesList = snd checkForCollisions

        newHeartsThere = removeTheHeartsLost heartsThere newHP2
        newAllHeartPictures = updateAllHeartPics allheartPictures newHeartsThere pics

        -- newBonePic = if newTime >= 0.75 then Rotate 30 bonePic else bonePic

        newScore = if length (newZombiesList) < length enimies
            then score + 1
            else score

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
                HP {health = newHP2, heartPositions = hposs, heartPictures = pics, heartIsThere = newHeartsThere, allheartPictures = newAllHeartPictures}
                newInx
                cw {bullets = newBulletsList}
                newLevels
                levelIndex
                newGameOverState
                gameOverPic
                hb
                zaf
                screenState
                updateKnightAttacking
                bulletImage
                newfinalBones
                newScore
                bonePic
                (boneAngle + 30)
                boss

-- function to despawn the bones on the outside
despawnOutOfBoundsBones :: Float -> Float -> [Bullet] -> [Bullet]
despawnOutOfBoundsBones height width bones = [b | b <- bones, let (bx,by) = position b, checkBounds (bx,by) height width]

-- check if a position is within the height and width 
checkBounds :: (Float, Float) -> Float -> Float -> Bool
checkBounds (bx, by) height width
            | bx < -(width/2) = False
            | bx > (width/2) = False
            | by > (height/2) = False
            | by < -(height/2) = False
            | otherwise = True

-- function to update the state for the story mode 
updateWorldState4Story :: Float -> WorldState -> WorldState
updateWorldState4Story steps (WorldState (x,y) clr radius b c enimies window timeAcc frames i z background kh dir 
                             (HP health hposs pics heartsThere allheartPictures) psi cw lvls levelIndex isGameOver 
                             gameOverPic hb zaf screenState knightInfo bulletImage zombieBones score bonePic boneAngle boss) =

    let newTime = (steps + timeAcc)
        (newInx, resetTime) = if newTime >= 0.5
                                    then ((i + 1), 0)
                                    else (i, newTime)

        newCharacterPos = changePosBasedUponKeyHolds (x,y) kh window


        newEnimies = moveEnimiesCloser enimies (x,y) 1.5


        
        attackingZombies = getAttackingZombies enimies (x,y)
        randgedAttackingZombies = getAttackingZombies2 enimies (x,y)

        (newZombieIndex, resetZombieTimer) = if newTime >= 0.25
                                                    then (1, 0)
                                                    else (0, newTime)
        updateZombies =
                    [ if z `elem` attackingZombies
                    then z {isAttacking = True, zAttackTimer = 0.5, zombieAttackingFramesIndex = zombieAttackingFramesIndex z + newZombieIndex}
                    else
                        let newTimer = max 0 (zAttackTimer z - steps)
                        in z {isAttacking = newTimer > 0, zAttackTimer = resetZombieTimer, zombieAttackingFramesIndex = 0}
                        | z <- newEnimies ]

        updateZombies2 =  [ if z `elem` randgedAttackingZombies
                            then (
                                if timeSinceShot z > 1.0
                                then z {timeSinceShot = 0, isReadyToShoot = True}
                                else let newTimeSince = max 0.0 (timeSinceShot z + steps)
                                     in  z {timeSinceShot = newTimeSince})

                            else z | z <- updateZombies]

        newZombieBones = spawnNewBones zombieBones (x,y) [z | z <- updateZombies2, isReadyToShoot z ]

        updateBoss =  
                     if rangedAttackTimer boss > 0.5
                     then boss {rangedAttackTimer = 0, isRdy2Shoot = True}
                     else let newTimeSince = max 0.0 (rangedAttackTimer boss + steps)
                          in  boss {rangedAttackTimer = newTimeSince}
        
        updateBoss1 =  
                     if specialAttackTimer updateBoss > 1.0
                     then updateBoss {specialAttackTimer = 0, isRdy2Shoot2 = True}
                     else let newTimeSince1 = max 0.0 (specialAttackTimer updateBoss + steps)
                          in  updateBoss {specialAttackTimer = newTimeSince1}

        newFireBalls = if isBossAlive updateBoss1 then spawnNewFireBalls (fireBalls updateBoss1) (x,y) updateBoss1 else fireBalls updateBoss1
        newBones = if isBossAlive updateBoss1 then (spawnEightBonesForBoss newZombieBones updateBoss1) else newZombieBones

        updateZombies3 = [z {isReadyToShoot = False} | z <- updateZombies2]

        updateBoss2 = (updateBoss1) {isRdy2Shoot = False, isRdy2Shoot2 = False}



        (newKnightIndex, resetKnightTimer) = if newTime >= 1
                                             then (1, 0)
                                             else (0, newTime)
        updateKnightAttacking =
            if knightIsAttacking (knightInfo)
            then
                let newTimer = max 0.0 (knightAttackTimer knightInfo - steps)
                    stillAttacking = newTimer > 0
                    totalAttackDuration = 1.3
                    frameDuration = totalAttackDuration / fromIntegral (length (knightAttackFrames knightInfo))
                    elapsed = totalAttackDuration - newTimer

                    frame = floor (elapsed / frameDuration)
                    newIndex = if stillAttacking then frame else 0
                in knightInfo {knightIsAttacking = stillAttacking, knightAttackTimer = resetKnightTimer, knightAttackFrameIndex = newIndex }
            else
                knightInfo


        newHp =
                    if newTime >= 0.5
                    then health - length (getAttackingZombies enimies (x, y))
                    else health

        newGameOverState = newHp == 0

        newBulletsPos = updateBulletsPos (bullets cw)
        newBonesPos = updateBulletsPos newBones
        newFireBallPos = updateBulletsPos newFireBalls

        -- initWorld <- initialWorld
        (newLvlIndex, newScreenState, newHp1)
          | levelIndex > 6 = (1, screenState {mainMenu = True, playingEndlessScreen = False, playingStoryScreen = False}, 5)
          | (null updateZombies3 && not (dialouge screenState) && not (isBossAlive updateBoss2)) = (levelIndex, screenState{dialouge = True, playingStoryScreen = False}, newHp)
          | otherwise = (levelIndex, screenState, newHp)

        checkForCollisions = checkForBulletCollisions (newBulletsPos) updateZombies3
        
        (newBonesL, newHP2) = checkForBulletCollisions2 (newBonesPos) (x,y) newHp1
        (newFireBalls1, newHP3) = checkForBulletCollisions2 newFireBallPos (x,y) newHP2

        newfinalBones = (despawnOutOfBoundsBones (height window) (width window) newBonesL)
        newBulletsList = fst checkForCollisions
        newZombiesList = snd checkForCollisions

        (newBullets2, newBoss) = checkForBulletCollisions1 (newBulletsList) updateBoss2

        newHeartsThere = removeTheHeartsLost heartsThere newHP3
        newAllHeartPictures = updateAllHeartPics allheartPictures newHeartsThere pics

        -- newBonePic = if newTime >= 0.75 then Rotate 30 bonePic else bonePic
        newScore = if length (newZombiesList) < length enimies
                   then score + 1
                   else score

        newBoss20 = if healthB newBoss <= 0 then (newBoss) {isBossAlive = False} else newBoss

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
                HP {health = newHP3, heartPositions = hposs, heartPictures = pics, heartIsThere = newHeartsThere, allheartPictures = newAllHeartPictures}
                newInx
                cw {bullets = newBullets2}
                lvls
                newLvlIndex
                newGameOverState
                gameOverPic
                hb
                zaf
                newScreenState
                updateKnightAttacking
                bulletImage
                newfinalBones
                newScore
                bonePic
                (boneAngle + 30)
                newBoss20 {fireBalls = newFireBalls1}
 

-- function to update the levels 
updateLevels ::  [Levels] -> Int -> Levels -> [Levels]
updateLevels levels indxOfOneToUpdate lvl1 = [if indx == indxOfOneToUpdate then lvl1 else lvl | (indx, lvl) <- zip [0..] levels]

-- function to update the bullets positions
updateBulletsPos :: [Bullet] -> [Bullet]
updateBulletsPos bullets = [b {position = (x + vx, y + vy)} | b <- bullets, let (vx, vy) = (velocity b), let (x, y) = (position b)]

-- function to check for collisions between zombies and bullets
checkForBulletCollisions :: [Bullet] -> [Zombie] -> ([Bullet], [Zombie])
checkForBulletCollisions bullets zombies =
    let hits = [(b, z) | b <- bullets, z <- zombies, distance (position b) (zPos z) < if isBossZombie z then 60 else 30]
        newBullets = [b | (b, _) <- hits]
        newZombies = [if any (\(_, z1) -> z == z1) hits
                      then z {zombieHP = zombieHP z - 1}
                      else z | z <- zombies]
        survivingZombies = filter (\z -> zombieHP z > 0) newZombies
    in ([b | b <- bullets, b `notElem` newBullets], survivingZombies)

-- function to check for collisions between the bones and the player
checkForBulletCollisions2 :: [Bullet] -> (Float, Float) -> Int -> ([Bullet], Int)
checkForBulletCollisions2 bullets (px, py) playerHp =
    let hits = [(b, playerHp - damagei) | b <- bullets, let damagei = damage b, distance (position b) (px, py) < 30]
        newBullets = [b | (b, _) <- hits]
    in ([b | b <- bullets, b `notElem` newBullets], max 0 playerHp - length hits)

-- function to check for collisions between the bullets and the boss
checkForBulletCollisions1 :: [Bullet] -> Boss -> ([Bullet], Boss)
checkForBulletCollisions1 bullets boss =
    let hits = [b | b <- bullets, distance (position b) (positionB boss) < 100]
        newBullets = [b | b  <- hits]
    in ([b | b <- bullets, b `notElem` newBullets], boss {healthB = (healthB boss) - length hits})


-- function for the angle to the player 
angle2Player :: (Float, Float) -> (Float, Float) -> Float
angle2Player (px, py) (zx, zy) = atan2 (py - zy) (px - zx)

-- function to update the heart pictures 
updateAllHeartPics :: [Picture] -> [Bool] -> [Picture] -> [Picture]
updateAllHeartPics allheartPictures isHeartThere avalibleOptions = [ if x then avalibleOptions !! 1 else avalibleOptions !! 0 | x <- isHeartThere]

-- function to update the position based upon if keys are held or not
changePosBasedUponKeyHolds :: (Float, Float) -> KeysHeld -> WindowDimensions -> (Float, Float)
changePosBasedUponKeyHolds (playerX, playerY) (K isUpHeld isDownHeld isLeftHeld isRightHeld) (Window width height) =
     let
        dyUp    = if isUpHeld    && playerY + 3 <  height / 2 then  3 else 0
        dyDown  = if isDownHeld  && playerY - 3 > - (height / 2) then -3 else 0
        dxRight = if isRightHeld && playerX + 3 <  width  / 2 then  3 else 0
        dxLeft  = if isLeftHeld  && playerX - 3 > - (width / 2) then -3 else 0

        dx = dxLeft + dxRight
        dy = dyUp + dyDown
    in (playerX + dx, playerY + dy)

-- Function to move the enimies a little bit closer to the player 
moveEnimiesCloser :: [Zombie] -> (Float, Float) -> Float -> [Zombie]
moveEnimiesCloser zombies (playerX, playerY) speed =

    [if sqrt (dx^2   +  dy^2) > cap  -- && not (checkIfTooCloseToOtherEnimies enimies (ex, ey))
        then z {zPos = (ex + (dx/ sqrt (dx^2   +  dy^2)) * speed, ey + (dy/ sqrt (dx^2   +  dy^2)) * speed)} -- normalized vectors times the speed
        else z  -- dont do anything
        | z <- zombies,
     let (ex, ey) = zPos z,
     let dx = (playerX - ex),
     let dy = (playerY - ey),
     let isRanged = isRangedZombie z,
     let cap = if isRanged then 500 else 50
     ]

-- function to draw the zombie hitboxes 
drawZombieHitBoxes :: [Zombie] -> [Picture]
drawZombieHitBoxes zombies =

    [Translate x y (zombieHitBox) | z <- zombies, let (x, y) = (zPos z), let isBoss = isBossZombie z,
    let zombieHitBox
          | zombieHP z <= 2 = (if isBoss
                          then Scale 2.0 2.0 (Color red (Circle 30))
                          else (Color red (Circle 30)))
          | isBoss = Scale 2.0 2.0 (Color orange (Circle 30))
          | otherwise = Color orange (Circle 30)]

-- function to load in the zombies frames
loadInZombieFrames :: IO [Picture]
loadInZombieFrames = sequence [loadBMP ("assets/zombieFrame" ++ show x ++ ".bmp") | x <- [1..2]]

-- function to load in the players frames
loadInPlayerFrames :: IO [Picture]
loadInPlayerFrames = sequence [loadBMP ("assets/knightFrame" ++ show x ++ ".bmp") | x <- [1..2]]

-- Heart 1 = Empty , Heart 2 = Full
loadInHealthFrames :: IO [Picture]
loadInHealthFrames = sequence [loadBMP ("assets/Heart" ++ show x ++ ".bmp") | x <- [1..2]]

-- function to load in zombie attacking frames
loadInZombieAttackingFrames :: IO [Picture]
loadInZombieAttackingFrames = sequence [loadBMP ("assets/zombieAttackFrame" ++ show x ++ ".bmp") | x <- [1..9]]

-- function to load in player attacking frames
loadInPlayerAttackingFrames :: IO [Picture]
loadInPlayerAttackingFrames = sequence [loadBMP ("assets/knightAttackFrame" ++ show x ++ ".bmp") | x <- [1..14]]

-- Hotbar 1 = HotBar 2 = 
loadInHotBarAssets :: IO [Picture]
loadInHotBarAssets = sequence [loadBMP ("assets/hotBar" ++ show x ++ ".bmp") | x <- [1..2]]

-- function to load in the backgrounds 
loadInBackGrounds :: IO [Picture]
loadInBackGrounds = sequence [loadBMP ("assets/backGround" ++ show x ++ ".bmp") | x <- [1..8]]


initialWorld :: IO WorldState
initialWorld =
    do
        mainCharacter <- loadInPlayerFrames
        gameOverPicture <- loadBMP "assets/youDied.bmp"
        z <- loadInZombieFrames
        backgrounds <- loadInBackGrounds
        heartPictures <- loadInHealthFrames
        hotBarPictures <- loadInHotBarAssets
        zombieAttackFrames <- loadInZombieAttackingFrames
        knightAttackingFrames <- loadInPlayerAttackingFrames
        bullet <- loadBMP "assets/Arrow.bmp"
        boneAsset <- loadBMP "assets/boneAsset.bmp"
        diagBack <- loadBMP "assets/dialougeScreen.bmp"
        fireBall <- loadBMP "assets/fireBall.bmp"
        bossPic <- loadBMP "assets/bossAsset.bmp"



        let initialHeart = heartPictures !! 1
        let windowHeight = 1000
        let windowWidth = 1800

         in return (WorldState
                    (0, 0)
                    green
                    20
                    False
                    0
                    []
                    (Window windowWidth windowHeight)
                    0
                    mainCharacter
                    0
                    z
                    backgrounds
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
                    [L {levelEnemyAmmount = 5, levelInterval = 5, totalTimeSpawning = 30, spawnT = 0},
                    L {levelEnemyAmmount = 10, levelInterval = 2.5, totalTimeSpawning = 35, spawnT = 0}]
                    1
                    False
                    gameOverPicture
                    H {hotBarPictures= hotBarPictures, hotBarIndex= 1}
                    zombieAttackFrames
                    Screen
                    {mainMenu = True,
                    playingEndlessScreen = False,
                    playingStoryScreen = False,
                    endOfStoryScreen = False,
                    dialouge = False,
                    diag1Txt = ["Who dares challenge the glorious crypt of Glasgow?!"],
                    diag2Txt = ["Oh? You lived, impressive...", "See if you can beat this then!"],
                    diag3Txt = ["You think you alone can defeat me?", "When countless others have failed??"],
                     diag4Txt = ["I have slumbered in this crypt for 1000 years!", "I have killed all who dare enter!", "My army cannot be bested by a single man!"],
                     diag5Txt = ["Argh, fine! I will do this myself!!", "I am the glorious necromancer Glasgow...", "I cannot be defeated!!"],
                     diag6Txt = ["No, this is impossible!", "I will be back, this I promise!", "Count your days Joseph Haugh!!"],
                     dialougeBackGround = diagBack
                     }
                    Knight {knightAttackFrames = knightAttackingFrames, knightIsAttacking = False, knightAttackTimer = 0, knightAttackFrameIndex = 0}
                    bullet
                    []
                    0
                    boneAsset
                    0
                    Bozz {healthB = 1, positionB = (0,300), rangedAttackTimer = 0.0, specialAttackTimer = 0.0, bossFireballSprite = fireBall, bossPicture = bossPic, isBossAlive = False, fireBalls = [], isRdy2Shoot = False, isRdy2Shoot2 = False}
                    )

main :: IO ()
main = do
    initialWorld1 <- initialWorld
    play
        FullScreen
        black
        60
        initialWorld1
        drawWorld
        handleInput
        tempFun
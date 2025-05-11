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
    -- zombieMoveFrames :: [Picture],
    zombieAttackingFramesIndex :: Int,
    zombieHP :: Int,
    isBossZombie :: Bool,
    isRangedZombie :: Bool
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
    endOfStoryScreen :: Bool
}

data KnightInformation = Knight {
    knightAttackFrames :: [Picture],
    knightIsAttacking :: Bool,
    knightAttackTimer :: Float,
    knightAttackFrameIndex :: Int
}

spawnNewEnemies :: Int -> [Zombie]
spawnNewEnemies x = case x of
        1 -> [Z{zPos =(275, 250),isAttacking = False ,zAttackTimer= 0, zombieHP = 2, zombieAttackingFramesIndex = 0, isBossZombie = False, isRangedZombie = False},
              Z{zPos =(400, 0),isAttacking = False ,zAttackTimer= 0, zombieHP = 2, zombieAttackingFramesIndex = 0, isBossZombie = False, isRangedZombie = False},
              Z{zPos =(275, -250),isAttacking = False ,zAttackTimer= 0, zombieHP = 2, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False}]

        2 -> [Z{zPos =(275, 250),isAttacking = False ,zAttackTimer= 0, zombieHP = 4, zombieAttackingFramesIndex = 0, isBossZombie = False, isRangedZombie = False },
              Z{zPos =(400, 0),isAttacking = False ,zAttackTimer= 0, zombieHP = 4, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False},
              Z{zPos =(275, -250),isAttacking = False ,zAttackTimer= 0, zombieHP = 4, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False },
              Z{zPos =(-275, -125),isAttacking = False ,zAttackTimer= 0, zombieHP = 4, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False},
              Z{zPos =(-275, 125),isAttacking = False ,zAttackTimer= 0, zombieHP = 4, zombieAttackingFramesIndex = 0, isBossZombie = False, isRangedZombie = False}]

        3 -> [Z{zPos =(275, 250),isAttacking = False ,zAttackTimer= 0, zombieHP = 6, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False},
              Z{zPos =(400, 0),isAttacking = False ,zAttackTimer= 0, zombieHP = 6, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False},
              Z{zPos =(275, -250),isAttacking = False ,zAttackTimer= 0, zombieHP = 6, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False},
              Z{zPos =(-275, -125),isAttacking = False ,zAttackTimer= 0, zombieHP = 6, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False},
              Z{zPos =(-275, 125),isAttacking = False ,zAttackTimer= 0, zombieHP = 6, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False},
              Z{zPos =(-275, 250),isAttacking = False ,zAttackTimer= 0, zombieHP = 6, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False},
              Z{zPos =(-400, 0),isAttacking = False ,zAttackTimer= 0, zombieHP = 6, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False},
              Z{zPos =(-275, -250),isAttacking = False ,zAttackTimer= 0, zombieHP = 6, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False},
              Z{zPos =(275, -125),isAttacking = False ,zAttackTimer= 0, zombieHP = 6, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False},
              Z{zPos =(275, 125),isAttacking = False ,zAttackTimer= 0, zombieHP = 6, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False}]
        
        4 -> [Z{zPos =(275, 250),isAttacking = False ,zAttackTimer= 0, zombieHP = 8, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False},
              Z{zPos =(400, 0),isAttacking = False ,zAttackTimer= 0, zombieHP = 25, zombieAttackingFramesIndex = 0, isBossZombie = True,  isRangedZombie = False},
              Z{zPos =(275, -250),isAttacking = False ,zAttackTimer= 0, zombieHP = 8, zombieAttackingFramesIndex = 0, isBossZombie = False,  isRangedZombie = False}]

        

handleInput :: Event -> WorldState ->  WorldState

-- you want to choose the endless mode
handleInput (EventKey (Char 'e') Down _ _) worldState = 
                worldState {screenState = (screenState worldState) {mainMenu = False, playingEndlessScreen = True, playingStoryScreen = False}}
-- you want to choose the storyMode
handleInput (EventKey (Char 's') Down _ _) worldState =
    worldState {screenState = (screenState worldState) {mainMenu = False, playingEndlessScreen = False, playingStoryScreen = True}}

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

-- Case for if you let go of the upkey
handleInput (EventKey (SpecialKey KeyUp) Up _ _) worldState =
    worldState {keysHeld = (keysHeld worldState) {isUpHeld = False}}
   
        

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

-- Case for if you let go of the downKey
handleInput (EventKey (SpecialKey KeyDown) Up _ _) worldState =
    worldState {keysHeld = (keysHeld worldState) {isDownHeld = False}}

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
        
-- Case for if you let go of the leftKey
handleInput (EventKey (SpecialKey KeyLeft) Up _ _) worldState =
    worldState {keysHeld = (keysHeld worldState) {isLeftHeld = False}}


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

-- Case for if you let go of the leftKey
handleInput (EventKey (SpecialKey KeyRight) Up _ _) worldState =
    worldState {keysHeld = (keysHeld worldState) {isRightHeld = False}}

-- Case for if you let go of the rightKey
handleInput (EventKey (SpecialKey KeySpace) Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background k dir health psi (W closeAttack rangedAttack numOfBullets bullets) lvls levelIndex isGameOver gameOverPic hb zaf screenState knightInfo bulletImage zombieBones score)  =

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



handleInput (EventKey (Char '1') Down _ _) worldState =
    worldState {currentWeapons = (currentWeapons worldState) {rangedAttack = False, closeAttack = True},
            hotbar = (hotbar worldState) {hotBarIndex = 0}}


handleInput (EventKey (Char '2') Down _ _) (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background kh dir health psi cw lvls levelIndex isGameOver gameOverPic hb zaf screenState knightInfo bulletImage zombieBones score) =

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

handleInput _ (WorldState (x,y) clr radius b c enimies (Window w h) timeAcc frames i z background kh dir health psi cw lvls levelIndex isGameOver gameOverPic hb zaf screenState knightInfo bulletImage zombieBones score) =
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


-- shootBullet :: Direction -> (Float, Float) -> [Bullet] -> 
-- function to calculate the HP lost 
getAttackingZombies :: [Zombie] -> (Float, Float) -> [Zombie]
getAttackingZombies zombies (px, py) = [z | z <- zombies, distance (zPos z) (px, py) < 50]

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
drawWorld worldState =
    case (mainMenu (screenState worldState), playingEndlessScreen (screenState worldState)) of
            (True, _) -> drawMainMenu worldState
            (False, True) -> drawPlayingEndlessScreen worldState
            (False, False) -> drawPlayingEndlessScreen worldState





drawMainMenu :: WorldState -> Picture
drawMainMenu (WorldState (x,y) clr radius _ _ enimies w timeAcc playerFrames i z b kh dir (HP health hposs pics heartsThere allheartPictures) psi (W closeAttack rangedAttack numOfBullets bullets ) lvls levelIndex isGameOver gameOverPic (H hotBarPictures hotBarIndex) zaf screenState knightInfo bulletImage zombieBones score) =
    Pictures [Scale 2.5 1.5 (b !! 1)]



drawPlayingEndlessScreen :: WorldState -> Picture
drawPlayingEndlessScreen (WorldState (x,y) clr radius _ _ enimies w timeAcc playerFrames i z b kh dir (HP health hposs pics heartsThere allheartPictures) psi (W closeAttack rangedAttack numOfBullets bullets ) lvls levelIndex isGameOver gameOverPic (H hotBarPictures hotBarIndex) zaf screenState knightInfo bulletImage zombieBones score) =

    let currentZombieFrame = z !! (i `mod` length z)
        currentPlayerFrame = playerFrames !! (psi `mod` length playerFrames)
        angle2Rotate = getRotateBasedUponDir dir
        currentGameOver = if isGameOver then gameOverPic else Circle 0
        currentPlayerFrame2 = if knightIsAttacking knightInfo
                              then knightAttackFrames knightInfo !! knightAttackFrameIndex knightInfo
                              else currentPlayerFrame
        currentBackGround = if playingEndlessScreen screenState
                            then Scale 2.5 1.5 (b !! 0)
                            else Scale 1.75 1 (b !! (levelIndex + 1))

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
    drawZombieHitBoxes enimies ++
    [Scale 0.75 0.75 (Translate 0 (-(height w/2) + 25) (hotBarPictures !! hotBarIndex))] ++
    [Translate 0 0 currentGameOver])

drawBullets :: [Bullet] -> Picture -> [Picture]
drawBullets bullets bulletImage =  [Color black (translate x y (Rotate (getRotateBasedUponDir (direction b) + 180) (bulletImage))) | b <- bullets,
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
tempFun steps world@(WorldState (x,y) clr radius b c enimies window timeAcc frames i z background kh dir (HP health hposs pics heartsThere allheartPictures) psi cw lvls levelIndex isGameOver gameOverPic hb zaf screenState knightInfo bulletImage zombieBones score)

    | health <= 0 = world {isGameOver = True}
    | isGameOver = world
    | otherwise =

    case (mainMenu (screenState), playingEndlessScreen (screenState)) of
        (False, True) ->
            updateWorldState4Endless steps world
        (True, False) ->
            (WorldState (x,y) clr radius b c enimies window timeAcc frames i z background kh dir (HP health hposs pics heartsThere allheartPictures) psi cw lvls levelIndex isGameOver gameOverPic hb zaf screenState knightInfo bulletImage zombieBones score)
        (False, False) ->
            updateWorldState4Story steps world

updateWorldState4Endless :: Float -> WorldState -> WorldState
updateWorldState4Endless steps (WorldState (x,y) clr radius b c enimies window timeAcc frames i z background kh dir (HP health hposs pics heartsThere allheartPictures) psi cw lvls levelIndex isGameOver gameOverPic hb zaf screenState knightInfo bulletImage zombieBones score) =
    let newTime = (steps + timeAcc)
        (newInx, resetTime) = if newTime >= 0.5
                              then ((i + 1), 0)
                              else (i, newTime)
        newCharacterPos = changePosBasedUponKeyHolds (x,y) kh window
        newEnimies = moveEnimiesCloser enimies (x,y) 1.5

        attackingZombies = getAttackingZombies enimies (x,y)

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
                    then (Z{zPos =(1000, -500),isAttacking = False ,zAttackTimer= 0, zombieHP = 2, zombieAttackingFramesIndex = 0, isBossZombie = False, isRangedZombie = False} : updateZombies, 0)
                    else (updateZombies, newSpawnT)

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
                newGameOverState
                gameOverPic
                hb
                zaf
                screenState
                updateKnightAttacking
                bulletImage
                zombieBones
                score

updateWorldState4Story :: Float -> WorldState -> WorldState
updateWorldState4Story steps (WorldState (x,y) clr radius b c enimies window timeAcc frames i z background kh dir (HP health hposs pics heartsThere allheartPictures) psi cw lvls levelIndex isGameOver gameOverPic hb zaf screenState knightInfo bulletImage zombieBones score) =
    let newTime = (steps + timeAcc)
        (newInx, resetTime) = if newTime >= 0.5
                                    then ((i + 1), 0)
                                    else (i, newTime)

        newCharacterPos = changePosBasedUponKeyHolds (x,y) kh window
        newEnimies = moveEnimiesCloser enimies (x,y) 1.5

        attackingZombies = getAttackingZombies enimies (x,y)

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
        

        newLvlIndex = if null updateZombies 
                    then levelIndex + 1
                    else levelIndex

        -- if newLvlIndex > 3 then 

        newZombies = if null updateZombies 
                    then spawnNewEnemies newLvlIndex
                    else updateZombies

        checkForCollisions = checkForBulletCollisions (newBulletsPos) newZombies

        newBulletsList = fst checkForCollisions
        newZombiesList = snd checkForCollisions
        newHeartsThere = removeTheHeartsLost heartsThere newHp
        newAllHeartPictures = updateAllHeartPics allheartPictures heartsThere pics

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
                HP {health = newHp, heartPositions = hposs, heartPictures = pics, heartIsThere = newHeartsThere, allheartPictures = newAllHeartPictures}
                newInx
                cw {bullets = newBulletsList}
                lvls
                newLvlIndex
                newGameOverState
                gameOverPic
                hb
                zaf
                screenState
                updateKnightAttacking
                bulletImage
                zombieBones
                newScore
       
    


updateLevels ::  [Levels] -> Int -> Levels -> [Levels]
updateLevels levels indxOfOneToUpdate lvl1 = [if indx == indxOfOneToUpdate then lvl1 else lvl | (indx, lvl) <- zip [0..] levels]

updateBulletsPos :: [Bullet] -> [Bullet]
updateBulletsPos bullets = [b {position = (x + vx, y + vy)} | b <- bullets, let (vx, vy) = (velocity b), let (x, y) = (position b)]


checkForBulletCollisions :: [Bullet] -> [Zombie] -> ([Bullet], [Zombie])
checkForBulletCollisions bullets zombies =
    let hits = [(b, z) | b <- bullets, z <- zombies, distance (position b) (zPos z) < if isBossZombie z then 60 else 30]
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
drawZombieHitBoxes zombies = 
    
    [Translate x y (zombieHitBox) | z <- zombies, let (x, y) = (zPos z), let isBoss = isBossZombie z, 
    let zombieHitBox = if zombieHP z <= 2 
                       then 
                          (if isBoss 
                          then Scale 2.0 2.0 (Color red (Circle 30)) 
                          else (Color red (Circle 30))) 
                        else
                          (if isBoss 
                          then Scale 2.0 2.0 (Color orange (Circle 30)) 
                          else (Color orange (Circle 30)))]

-- function to load in the zombies frames
loadInZombieFrames :: IO [Picture]
loadInZombieFrames = sequence [loadBMP ("assets/zombieFrame" ++ show x ++ ".bmp") | x <- [1..2]]

-- function to load in the players frames
loadInPlayerFrames :: IO [Picture]
loadInPlayerFrames = sequence [loadBMP ("assets/knightFrame" ++ show x ++ ".bmp") | x <- [1..2]]

-- Heart 1 = Empty , Heart 2 = Full
loadInHealthFrames :: IO [Picture]
loadInHealthFrames = sequence [loadBMP ("assets/Heart" ++ show x ++ ".bmp") | x <- [1..2]]

loadInZombieAttackingFrames :: IO [Picture]
loadInZombieAttackingFrames = sequence [loadBMP ("assets/zombieAttackFrame" ++ show x ++ ".bmp") | x <- [1..9]]

loadInPlayerAttackingFrames :: IO [Picture]
loadInPlayerAttackingFrames = sequence [loadBMP ("assets/knightAttackFrame" ++ show x ++ ".bmp") | x <- [1..14]]

-- Hotbar 1 = HotBar 2 = 
loadInHotBarAssets :: IO [Picture]
loadInHotBarAssets = sequence [loadBMP ("assets/hotBar" ++ show x ++ ".bmp") | x <- [1..2]]

loadInBackGrounds :: IO [Picture]
loadInBackGrounds = sequence [loadBMP ("assets/backGround" ++ show x ++ ".bmp") | x <- [1..7]]

main :: IO ()
main = do
    mainCharacter <- loadInPlayerFrames
    gameOverPicture <- loadBMP "assets/youDied.bmp"
    z <- loadInZombieFrames
    backgrounds <- loadInBackGrounds
    heartPictures <- loadInHealthFrames
    hotBarPictures <- loadInHotBarAssets
    zombieAttackFrames <- loadInZombieAttackingFrames
    knightAttackingFrames <- loadInPlayerAttackingFrames
    bullet <- loadBMP "assets/Arrow.bmp"

    let initialHeart = heartPictures !! 1
    let windowHeight = 1000
    let windowWidth = 1875

    let initialWorld =  WorldState
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
                    Screen {mainMenu = True, playingEndlessScreen = False, playingStoryScreen = False, endOfStoryScreen = False}
                    Knight {knightAttackFrames = knightAttackingFrames, knightIsAttacking = False, knightAttackTimer = 0, knightAttackFrameIndex = 0}
                    bullet
                    []
                    0

    play
        FullScreen
        black
        60
        initialWorld
        drawWorld
        handleInput
        tempFun
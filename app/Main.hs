module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display

import Lib

type Size     = Float
type Velocity = (Float, Float)

width :: (Ord a, Num a) => a
width = 16 * 100

height :: (Ord a, Num a) => a
height = 9 * 100

debugMode :: Bool 
debugMode = True

data Me = Me Size Location Velocity -- could also just be a generic Fish
    deriving (Eq, Show)
data Fish = Fish Size Location Velocity
    deriving (Eq, Show)

data Lake = Play [Fish] Me 
    | GameOver 
    | Winning
    deriving (Eq, Show)

initialLake :: Lake
initialLake = Play 
    [ Fish 3 (60, 200) (0,6)
    , Fish 13 (-55, 100) (2, -8)
    , Fish 20 (-70, 20) (2, 12)
    , Fish 5 (200, 400) (3,5)
    , Fish 4 (100, 200) (25, 25)
    ]
    (Me 10 (0, 0) (0,0))

drawWorld :: Lake -> Picture
drawWorld GameOver
    = Pictures [scale 0.25 0.25
     . translate (-700) 0
     . color white
     . text 
     $ "Oh no, you were eaten :(",
      scale 0.25 0.25
     . translate (-700) (-150)
     . color white
     . text 
     $ "Press F1 to play again"
    ]
drawWorld Winning
    = Pictures [scale 0.25 0.25
     . translate (-1500) 0
     . color white
     . text 
     $ "You ate everyone and ruined the entire ecosystem :)",
     scale 0.25 0.25
     . translate (-1500) (-150)
     . color white
     . text 
     $ "Press F1 to play again"]

drawWorld (Play fishies (Me size (x,y) (vx,vy)))
    = pictures [me, fish, debugScreen]
     where
        me      = color black (pictures [translate x y (circle size)])
        fish    = pictures [translate x y (color green (circle s)) | Fish s (x,y) _ <- fishies]
        debugScreen = scale 0.1 0.1 . translate (width*5) (height*5.5) . color white . text $ debugTexts 
        debugTexts = if debugMode then show width ++ "x" ++ show height ++ " x: " ++ show (ceiling x) ++ " y: " ++ show (ceiling y) else ""   

simulateLake :: Float -> (Lake -> Lake)
simulateLake _ GameOver     = GameOver
simulateLake _ Winning      = Winning
simulateLake timeStep (Play fishies me@(Me mySize myPos myVel@(x,y)))
    | any (collidesWithBiggerFish myPos mySize) fishies = GameOver 
    | any (collidesWithSmallerFish myPos mySize) fishies = Play (map updateFishie (updatedFishiesWhenEaten myPos fishies mySize))
                        (Me (mySize + 5) myNewPos myVel) 
    | fishies == [] = Winning                    
    | otherwise = Play (map updateFishie fishies)
                        (Me mySize myNewPos myNewVel)
    where 
        myNewPos :: Location 
        myNewPos = myPos .+ timeStep .* myNewVel

        myNewVel :: Velocity 
        myNewVel = case (checkIfHittingWalls myPos) of 
            (False, False) -> myVel
            (True, False) -> (-x, y)
            (False, True) -> (x, -y)
            (True, True)  ->  -1*(myVel)

        collidesWithBiggerFish :: Location -> Size -> Fish -> Bool
        collidesWithBiggerFish l s (Fish fs fl _) = (isInSamePosition l fl fs s) && (fs >= s) 

        -- todo optional : fish of same size are treated differently?
        collidesWithSmallerFish :: Location -> Size -> Fish -> Bool
        collidesWithSmallerFish l s (Fish fs fl _) = (isInSamePosition l fl fs s) && (fs < s) 

        isInSamePosition :: Location -> Location -> Size -> Size -> Bool
        isInSamePosition mylocation fishlocation fishSize mySize = magV (mylocation .- fishlocation) < (mySize + fishSize) -- note: size is always radius

        updateFishie :: Fish -> Fish
        updateFishie f@(Fish s l@(x, y) v)
                = if flatCheckIfHittingWalls l then Fish s l (0,0) else Fish s (l .+ timeStep .* v) v

        updatedFishiesWhenEaten :: Location -> [Fish] -> Size -> [Fish]
        updatedFishiesWhenEaten _ [] _ = []
        updatedFishiesWhenEaten myloc (f@(Fish s l v):xs) mySize = if (isInSamePosition myloc l s mySize) 
            then updatedFishiesWhenEaten myloc xs mySize
            else f:updatedFishiesWhenEaten myloc xs mySize
       
       
flatCheckIfHittingWalls :: Location -> Bool
flatCheckIfHittingWalls (x, y) = case checkIfHittingWalls (x,y) of 
    (False, False) -> False
    (_, _) -> True

checkIfHittingWalls :: Location -> (Bool, Bool)
checkIfHittingWalls (x, y) = (checkCoordinates x width, checkCoordinates y height)

checkCoordinates :: (Ord a, Num a, RealFloat a) => a -> a -> Bool
checkCoordinates n max
    | n < ((-max) *0.58) || n > (max*0.58)  = True -- todo: find out why this number is so arbitrary
    | otherwise  = False

handleEvents :: Event -> Lake -> Lake 
-- handleEvents (EventKey (MouseButton LeftButton) Down _ _) GameOver = initialLake
handleEvents (EventKey (SpecialKey KeyF1) Down _ _) GameOver = initialLake
handleEvents (EventKey (SpecialKey KeyF1) Down _ _) Winning = initialLake
handleEvents (EventKey (MouseButton LeftButton) Down _ clickPos)
        (Play fishies (Me s myPos myVel)) = Play fishies (Me s myPos newVel)
        where 
            newVel = countNewVel myVel myPos clickPos
handleEvents (EventKey (SpecialKey KeyF5) Down _ _) w = if debugMode then Winning else w   -- instant win with F5 when debugging          
handleEvents (EventKey (SpecialKey KeyF4) Down _ _) w = if debugMode then GameOver else w  -- instant loss with F4 when debugging           
handleEvents _ w = w            

countNewVel :: Velocity -> Location -> (Float, Float) -> Velocity            
countNewVel vel@(x, y) myPos clickPos
        | x < 350 || y < 350 = vel .+ (25 .* norm (myPos .- clickPos)) 
        | otherwise = vel

main :: IO ()
main = play 
        -- (InWindow "Kalapeli!" (width,height) (20,20)) 
        FullScreen
        (makeColorI 11 64 82 255)
        60 -- fps
        initialLake
        drawWorld 
        handleEvents
        simulateLake

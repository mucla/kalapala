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
    [ Fish 3 (40, 200) (0,6)
    , Fish 13 (-10, 100) (2, -8)
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
        debugScreen = scale 0.1 0.1 . translate (width*5.5) (height*5.5) . color white . text $ debugTexts 
        debugTexts = if debugMode then "debug" else ""   

simulateLake :: Float -> (Lake -> Lake)
simulateLake _ GameOver     = GameOver
simulateLake _ Winning      = Winning
simulateLake timeStep (Play fishies me@(Me mySize myPos myVel@(x,y)))
    | any (collidesWithBiggerFish myPos mySize) fishies = GameOver 
    | any (collidesWithSmallerFish myPos mySize) fishies = Play (map updateFishie (updatedFishiesWhenEaten myPos fishies))
                        (Me (mySize + 5) myNewPos myVel) 
    | fishies == [] = Winning                    
    | otherwise = Play (map updateFishie fishies)
                        (Me mySize myNewPos myNewVel)
    where 
        -- todo: check and fix 
        myNewPos :: Location 
        myNewPos = if checkIfHittingWalls myVel then (myPos) else (myPos .+ timeStep .* myVel) 

        myNewVel :: Location 
        myNewVel = if checkIfHittingWalls myVel then (-10,-10) else myVel

        collidesWithBiggerFish :: Location -> Size -> Fish -> Bool
        collidesWithBiggerFish l s (Fish fs fl _) = (isInSamePosition l fl fs) && (fs > s) 

        -- todo optional : fish of same size are treated differently?
        collidesWithSmallerFish :: Location -> Size -> Fish -> Bool
        collidesWithSmallerFish l s (Fish fs fl _) = (isInSamePosition l fl fs) && (fs < s) 

        isInSamePosition :: Location -> Location -> Size -> Bool
        isInSamePosition mylocation fishlocation fishsize = magV (mylocation .- fishlocation) < fishsize

        updateFishie :: Fish -> Fish
        updateFishie f@(Fish s l v@(x, y))
                = if checkIfHittingWalls v then Fish s l (0,0) else Fish s (l .+ timeStep .* v) v

        updatedFishiesWhenEaten :: Location -> [Fish] -> [Fish]
        updatedFishiesWhenEaten _ [] = []
        updatedFishiesWhenEaten myloc (f@(Fish s l v):xs) = if (isInSamePosition myloc l s) 
            then updatedFishiesWhenEaten myloc xs 
            else f:updatedFishiesWhenEaten myloc xs
       
       
checkIfHittingWalls :: Location -> Bool
checkIfHittingWalls (x, y) = (checkCoordinates x width) && (checkCoordinates y height)

checkCoordinates :: (Ord a, Num a, RealFloat a) => a -> a -> Bool
checkCoordinates x max
    | x < ((-max) * 0.5) = True
    | x > (max * 0.5)  = True
    | otherwise  = False

handleEvents :: Event -> Lake -> Lake 
-- handleEvents (EventKey (MouseButton LeftButton) Down _ _) GameOver = initialLake
handleEvents (EventKey (SpecialKey KeyF1) Down _ _) GameOver = initialLake
handleEvents (EventKey (SpecialKey KeyF1) Down _ _) Winning = initialLake
handleEvents (EventKey (MouseButton LeftButton) Down _ clickPos)
        (Play fishies (Me s myPos myVel)) = Play fishies (Me s myPos newVel)
        where 
            newVel  = myVel  .+ (50 .* norm (myPos .- clickPos))
handleEvents (EventKey (SpecialKey KeyF5) Down _ _) w = if debugMode then Winning else w   -- instant win with F5 when debugging          
handleEvents (EventKey (SpecialKey KeyF4) Down _ _) w = if debugMode then GameOver else w  -- instant loss with F4 when debugging           
handleEvents _ w = w            

main :: IO ()
main = play 
        -- (InWindow "Kalapeli!" (width,height) (20,20)) 
        FullScreen
        (makeColorI 11 64 82 255)
        24 
        initialLake
        drawWorld 
        handleEvents
        simulateLake

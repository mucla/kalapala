module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display

import Lib

type Size     = Float
type Velocity = (Float, Float)

width :: (Ord a, Num a) => a
width = 900

height :: (Ord a, Num a) => a
height = 900

data Me = Me Size Location Velocity -- could also just be a generic Fish
    deriving (Eq, Show)
data Fish = Fish Size Location Velocity
    deriving (Eq, Show)

data Lake = Play [Fish] Me 
    | GameOver 
    deriving (Eq, Show)

initialLake :: Lake
initialLake = Play 
    [ Fish 3 (40,500) (2,6)
    , Fish 15 (10, 100) (-2, -8)
    ]
    (Me 6 (0, 0) (0,0))

drawWorld :: Lake -> Picture
drawWorld GameOver
    = scale 0.3 0.3
     . translate (-400) 0
     . color white
     . text 
     $ "Oh no, you were eaten :("
drawWorld (Play fishies (Me size (x,y) (vx,vy)))
    = pictures [me, fish]
     where
        me      = color red (pictures [translate x y (circle size)])
        fish    = pictures [translate x y (color orange (circle s)) | Fish s (x,y) _ <- fishies]

simulateLake :: Float -> (Lake -> Lake)
simulateLake _ GameOver     = GameOver
simulateLake timeStep (Play fishies me@(Me mySize myPos myVel))
    | any (collidesWithBiggerFish myPos mySize) fishies = GameOver 
    | otherwise = Play (map updateFishie fishies)
                        (Me mySize myNewPos myVel)
    where 
        myNewPos :: Location 
        myNewPos = restoreToScreen (myPos .+ timeStep .* myVel)

        collidesWithBiggerFish :: Location -> Size -> Fish -> Bool
        collidesWithBiggerFish l s (Fish fs fl _) = (magV (l .- fl) < fs) && (fs > s) 

        updateFishie :: Fish -> Fish
        updateFishie f@(Fish s l v)
                = Fish s (restoreToScreen (l .+ timeStep .* v)) v

{- todo: destroy fishie after it's eaten >:)
        updateFishie :: Fish -> Me -> Maybe Fish
        updateFishie f@(Fish s l v) m@(Me myS _ _)
            | collidesWith f && s < myS = Nothing
            | otherwise
                = Just (Fish (s (restoreToScreen (fp .+ timeStep .* v)) v))
-}
       
restoreToScreen :: Location -> Location
restoreToScreen (x,y) = (cycleCoordinates x, cycleCoordinates y)

cycleCoordinates :: (Ord a, Num a) => a -> a
cycleCoordinates x 
    | x < (-475) = 950+x
    | x > 475    = x-950
    | otherwise  = x

handleEvents :: Event -> Lake -> Lake 
handleEvents (EventKey (MouseButton LeftButton) Down _ _) GameOver = initialLake
handleEvents (EventKey (MouseButton LeftButton) Down _ clickPos)
        (Play fishies (Me s myPos myVel)) = Play fishies (Me s myPos newVel)
        where 
            newVel  = myVel  .+ (50 .* norm (myPos .- clickPos))
handleEvents _ w = w            

main :: IO ()
main = play 
        (InWindow "Kalapeli!" (width,height) (20,20)) 
        (makeColorI 81 162 196 255)
        24 
        initialLake
        drawWorld 
        handleEvents
        simulateLake

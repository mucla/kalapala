module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display

import Lib

type Size     = Float
type Velocity = (Float, Float)

width :: (Ord a, Num a) => a
width = 1500

height :: (Ord a, Num a) => a
height = 1500

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

-- todo: destroy fishie after it's eaten >:)
{- 
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
    | x < (-400) = width+x
    | x > 400    = x-width
    | otherwise  = x

main :: IO ()
main = simulate (InWindow "Look at the fishies go!" (width,height) (20,20)) 
        black 
        24 
        initialLake
        drawWorld 
        (\view -> simulateLake)

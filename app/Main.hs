module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display

import Lib

type Size     = Float
type Location = (Float, Float)
type Velocity = (Float, Float)

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


main :: IO ()
main = display
        (InWindow "Drawing our initial world"
                  (550,550) 
                  (20,20))
        black 
        (drawWorld initialLake)

module Lib where

type Location = (Float, Float)

(.-) , (.+) :: Location -> Location -> Location
(x,y) .- (u,v) = (x-u,y-v)
(x,y) .+ (u,v) = (x+u,y+v)

(.*) :: Float -> Location -> Location
s .* (u,v) = (s*u,s*v)

infixl 6 .- , .+
infixl 7 .*

norm :: Location -> Location
norm (x,y) = let m = magV (x,y) in (x/m,y/m)

magV :: Location -> Float
magV (x,y) = sqrt (x**2 + y**2) 

limitMag :: Float -> Location -> Location
limitMag n pt = if (magV pt > n) 
                  then n .* (norm pt)
                  else pt

rotateV :: Float -> Location -> Location
rotateV r (x,y) = (x * cos r - y * sin r
                  ,x * sin r + y * cos r)




module ProjectToScreen
  ( Trapezium(..), Pole(..)
  , compTrapezium
  , visibleTrap
  , POV(..), getPOV,turnL,turnR
  ) where

import Wad (Seg(..),Vertex,V2(..),Thing(..))

data POV = POV -- (player's) point-of-view
  { pos :: Vertex -- TODO: use float-based world-pos here
  , angle :: Float
  -- TODO: add height here
  } deriving Show

turnL,turnR :: POV -> POV -- TODO: normalize angle
turnL POV{pos,angle} = POV { pos, angle = angle + 1 }
turnR POV{pos,angle} = POV { pos, angle = angle - 1 }

getPOV :: Thing -> POV
getPOV Thing{pos,angle} = POV {pos,angle = fromIntegral angle}

data Trapezium = Trapezium (Pole,Pole)
data Pole = Pole (Bool,Int,Int,Int) deriving Show

visibleTrap :: Trapezium -> Bool
visibleTrap (Trapezium(Pole(b1,_,_,_),Pole(b2,_,_,_))) =
  b1 && b2 -- completely visible
  -- TODO: support partially visible segs; clip

compTrapezium :: POV -> Seg -> Trapezium
compTrapezium pov seg = do
  let Seg{start,end} = seg
  let q1 = compXYY pov start
  let q2 = compXYY pov end
  Trapezium (q1,q2)

compXYY :: POV -> Vertex -> Pole
compXYY pov v1 = do
  let POV{pos=playerPos,angle=playerAngle} = pov
  let vScale = 3 -- TODO: what should this be?
  let (exFloorH,exCeilingH) = (32,88) -- TODO: use real values from sector
  let (screenW,screenH) = (320,200) -- TODO: from config
  let playerH = 50 -- TODO: from config; modified by current sector height
  let halfScreenW = screenW/2
  let halfScreenH = screenH/2
  let screenDistance = halfScreenW -- because FOV is +/- 45
  let p2v1 = v1 - playerPos
  let distV1 = distance p2v1
  let angleV1 = angleOfVec p2v1
  let screenAngleV1 = playerAngle - angleV1 -- onScreen is range +/- 45
  let onScreenV1 = inPOV screenAngleV1
  let pixFromCenterV1 = tan (deg2rad screenAngleV1) * halfScreenW
  let pixFromLeftV1 = halfScreenW + pixFromCenterV1
  let distScreenV1 = distanceF (V2 screenDistance (abs pixFromCenterV1))
  let dScale = distScreenV1/distV1
  let y1F = halfScreenH + vScale * dScale * (exFloorH - playerH)
  let y1C = halfScreenH + vScale * dScale * (exCeilingH - playerH)
  Pole (onScreenV1
       , floor pixFromLeftV1
       , floor y1F
       , floor y1C)

distance :: Vertex -> Float
distance v = distanceF (fmap fromIntegral v)

distanceF :: V2 Float -> Float
distanceF (V2 x y) = do
  sqrt (x * x + y * y)

angleOfVec :: Vertex -> Float
angleOfVec (V2 x y) =
  normAngle (rad2deg (atan2 (fromIntegral y) (fromIntegral x)))

normAngle :: Float -> Float
normAngle a =
  if a < 0 then a + 360 else a

rad2deg :: Float -> Float
rad2deg r = r*180/pi

deg2rad :: Float -> Float
deg2rad r = r*pi/180

inPOV :: Float -> Bool
inPOV a = a >= -45 && a <= 45 -- TODO: make FOV configurable

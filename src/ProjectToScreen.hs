
module ProjectToScreen
  ( Trapezium(..), Pole(..)
  , compTrapezium
  , visibleTrap
  , POV(..),getPOV,turnL,turnR,forwards,backwards,strafeL,strafeR
  ) where

import Wad (Wad(..),Level(..),Linedef(..),Sidedef(..),Seg(..),Vertex,V2(..),Thing(..),Sector(..))

data POV = POV -- (player's) point-of-view -- TODO: own module
  { pos :: V2 Float
  , angle :: Float
  -- TODO: add height here
  } deriving Show

turnL,turnR :: POV -> POV -- TODO: normalize angle
turnL POV{pos,angle} = POV { pos, angle = angle + 1 }
turnR POV{pos,angle} = POV { pos, angle = angle - 1 }

forwards :: POV -> POV
forwards POV{pos=V2 x y,angle} =
  POV { pos = V2 (x + cos (deg2rad angle) * stride) (y + sin (deg2rad angle) * stride)
      , angle }
  where stride = 5 -- TODO: config

backwards :: POV -> POV
backwards POV{pos=V2 x y,angle} =
  POV { pos = V2 (x - cos (deg2rad angle) * stride) (y - sin (deg2rad angle) * stride)
      , angle }
  where stride = 5

strafeL :: POV -> POV
strafeL POV{pos=V2 x y,angle} =
  POV { pos = V2 (x - sin (deg2rad angle) * stride) (y + cos (deg2rad angle) * stride)
      , angle }
  where stride = 5

strafeR :: POV -> POV
strafeR POV{pos=V2 x y,angle} =
  POV { pos = V2 (x + sin (deg2rad angle) * stride) (y - cos (deg2rad angle) * stride)
      , angle }
  where stride = 5

getPOV :: Thing -> POV
getPOV Thing{pos,angle} = POV
  { pos = fmap fromIntegral pos
  , angle = fromIntegral angle
  }

data Trapezium = Trapezium (Pole,Pole)
data Pole = Pole (Bool,Int,Int,Int) deriving Show

visibleTrap :: Trapezium -> Bool
visibleTrap (Trapezium(Pole(b1,_,_,_),Pole(b2,_,_,_))) =
  b1 && b2 -- completely visible
  -- TODO: support partially visible segs; clip

compTrapezium :: Wad -> POV -> Seg -> Trapezium
compTrapezium wad pov seg = do
  let Seg{start,end} = seg
  let sector = seg2sector wad seg
  let q1 = compXYY pov sector start
  let q2 = compXYY pov sector end
  Trapezium (q1,q2)

seg2sector :: Wad -> Seg -> Sector
seg2sector wad seg = do
  let Wad{level1=level} = wad
  let Level{sidedefs,sectors} = level
  let Seg{linedef} = seg
  let Linedef{frontSideId} = linedef
  let sidedef = sidedefs !! frontSideId
  let Sidedef{sectorId} = sidedef
  sectors !! sectorId

compXYY :: POV -> Sector -> Vertex -> Pole
compXYY pov sector v1 = do
  let POV{pos=playerPos,angle=playerAngle} = pov
  let Sector{floorH,ceilingH} = sector
  let exFloorH = fromIntegral floorH
  let exCeilingH = fromIntegral ceilingH
  let vScale = 1 -- TODO: what should this be?
  let (screenW,screenH) = (320,200) -- TODO: from config
  let playerH = 50 -- TODO: from config; modified by current sector height
  let halfScreenW = screenW/2
  let halfScreenH = screenH/2
  let screenDistance = halfScreenW -- because FOV is +/- 45
  let p2v1 :: V2 Float = fmap fromIntegral v1 - playerPos
  let distV1 = distance p2v1
  let angleV1 = angleOfVec p2v1
  let screenAngleV1 = playerAngle - angleV1 -- onScreen is range +/- 45
  let onScreenV1 = inPOV screenAngleV1
  let pixFromCenterV1 = tan (deg2rad screenAngleV1) * halfScreenW
  let pixFromLeftV1 = halfScreenW + pixFromCenterV1
  let distScreenV1 = distance (V2 screenDistance (abs pixFromCenterV1))
  let dScale = distScreenV1/distV1
  let y1F = halfScreenH + vScale * dScale * (exFloorH - playerH)
  let y1C = halfScreenH + vScale * dScale * (exCeilingH - playerH)
  Pole (onScreenV1
       , floor pixFromLeftV1
       , floor y1F
       , floor y1C)

distance :: V2 Float -> Float
distance (V2 x y) = do
  sqrt (x * x + y * y)

angleOfVec :: V2 Float -> Float
angleOfVec (V2 x y) =
  normAngle (rad2deg (atan2 y x))

normAngle :: Float -> Float
normAngle a =
  if a < 0 then a + 360 else a

rad2deg :: Float -> Float
rad2deg r = r*180/pi

deg2rad :: Float -> Float
deg2rad r = r*pi/180

inPOV :: Float -> Bool
inPOV a = a >= -45 && a <= 45 -- TODO: make FOV configurable

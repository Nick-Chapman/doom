
module ProjectToScreen
  ( Trapezium(..), Pole(..)
  , compTrapezium
  , visible
  , printTrapezium
  ) where

import Wad

data Trapezium = Trapezium (Pole,Pole)
data Pole = Pole (Bool,Int,Int,Int) deriving Show -- flag Pole!

visible :: Trapezium -> Bool
visible (Trapezium(Pole(b1,_,_,_),Pole(b2,_,_,_))) = b1 && b2

printTrapezium :: Trapezium -> IO ()
printTrapezium w = do
  let Trapezium(Pole(_,x1,y1f,y1c),Pole(_,x2,y2f,y2c)) = w
  print ((x1,y1f,y1c),(x2,y2f,y2c))

compTrapezium :: Wad -> Seg -> Trapezium
compTrapezium wad seg = do
  let Wad{level1} = wad
  let Level{vertexes} = level1
  let Seg{startId,endId} = seg
  let v1 = vertexes!!startId
  let v2 = vertexes!!endId
  let q1 = compXYY wad v1
  let q2 = compXYY wad v2
  Trapezium (q1,q2)

compXYY :: Wad -> Vertex -> Pole
compXYY wad v1 = do
  let vScale = 3 -- what should this be?
  let playerH  = 50
  let exFloorH = 32
  let exCeilingH = 88
  let screenWidth :: Float = 320
  let screenH :: Float = 200
  let halfScreenWidth = screenWidth/2
  let halfScreenH = screenH/2
  let screenDistance = halfScreenWidth -- because FOV is +/- 45
  let Wad{player=Thing{pos=playerPos,angle=playerAngleI}} = wad
  let playerAngle :: Float = fromIntegral playerAngleI
  let p2v1 = v1 - playerPos
  let distV1 = distance p2v1
  let angleV1 = angleOfVec p2v1
  let screenAngleV1 = playerAngle - angleV1 -- onScreen is range +/- 45
  let onScreenV1 = inPOV screenAngleV1
  let pixFromCenterV1 = tan (deg2rad screenAngleV1) * halfScreenWidth
  let pixFromLeftV1 = halfScreenWidth + pixFromCenterV1
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
inPOV a = a >= -45 && a <= 45


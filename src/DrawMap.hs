
module DrawMap
  ( draw
  ) where

import Pic
import Wad --(Wad(..),Level(..),Thing(..),Vertex,Int16,Node(..),Subsector(..),Seg(..),Linedef(..))
import Data.Word (Word16)
import Data.Bits (shiftL)
import ProjectToScreen -- (Trapezium(..),Pole(..),compTrapezium,visible,printTrapezium)


draw :: [Colour] -> Int -> Wad -> Pic ()
draw cols i wad = do
  let _ = draw2d wad
  draw3d cols i wad
  pure ()

-- Map view...
draw2d :: Wad -> Pic ()
draw2d wad = do
  let Wad{level1=level,player} = wad
  let Level{vertexes} = level
  let Thing{pos=playerPos} = player
  let segs = closestSegs level playerPos
  sequence_ [ do drawSeg vertexes col seg
            | seg <- segs
            , let col = if isPortal wad seg then grey else lightGrey
            ]
  drawPlayer 45 player


-- 1st person persepective...
draw3d :: [Colour] -> Int -> Wad -> Pic ()
draw3d randCols _i wad = do
  let Wad{level1=level,player} = wad
  let Thing{pos=playerPos} = player
  let segs = closestSegs level playerPos
  let vtraps =
        [ (seg,trap)
        | seg <- reverse segs -- start furthest away, for painter alg !!
        , let trap = compTrapezium wad seg
        , visible trap -- in players persepective
        , not (isPortal wad seg) -- solid wall
        ]
  --Mes (show ("#vtraps",length vtraps))
  sequence_ [ do draw3dsegSolid col trap
            | (col,(_,trap)) <- zip randCols vtraps
            ]
  -- highlight specific seg/trap
  let segI = _i `mod` length vtraps
  Eff (print segI)
  let (seg,trap) = vtraps!!segI
  Eff (_infoSeg wad seg)
  Eff (printTrapezium trap)
  let Level{vertexes=_vs} = level
  --drawSeg _vs green seg
  draw3dsegFrame yellow trap

isPortal :: Wad -> Seg -> Bool
isPortal wad seg = do
  let Wad{level1=level} = wad
  let Level{linedefs} = level
  let Seg{linedefId} = seg
  let linedef = linedefs!!(fromIntegral linedefId)
  let Linedef{backSideId} = linedef
  backSideId /= -1

_infoSeg :: Wad -> Seg -> IO ()
_infoSeg wad seg = do
  let Wad{level1=level} = wad
  let Level{linedefs} = level
  print seg
  let Seg{linedefId} = seg
  let linedef = linedefs!!(fromIntegral linedefId)
  print linedef
  pure ()

unquantize :: V2 Int16 -> V2 Float
unquantize = fmap fromIntegral

drawPlayer :: Int16 -> Thing -> Pic ()
drawPlayer h_fov Thing{pos,angle} = do
  Dot magenta (unquantize pos)
  let len = 3000
  let a = angle
  let a1 = a - h_fov
  let a2 = a + h_fov
  drawVec magenta pos a len
  drawVec magenta pos a1 len
  drawVec magenta pos a2 len

drawVec :: Colour -> Vertex -> Int16 -> Float -> Pic ()
drawVec col pos angle len = do
  let p = unquantize pos
  let r = radians angle
  let vec = V2 (len * cos r) (len * sin r)
  Line col p (p+vec)
  where
    radians n = fromIntegral n * pi / 180

drawSeg :: [Vertex] -> Colour -> Seg -> Pic ()
drawSeg vertexes col seg = do
  let Seg{startId,endId} = seg
  let start = vertexes!!startId
  let end = vertexes!!endId
  Line col (unquantize start) (unquantize end)

draw3dsegFrame :: Colour -> Trapezium -> Pic ()
draw3dsegFrame col w = do
  let Trapezium(Pole(_,x1,y1f,y1c),Pole(_,x2,y2f,y2c)) = w
  let a = V2 x1 y1f
  let b = V2 x2 y2f
  let c = V2 x1 y1c
  let d = V2 x2 y2c
  LineQ col a b
  LineQ col c d
  LineQ col a c
  LineQ col b d
  pure ()


draw3dsegSolid :: Colour -> Trapezium -> Pic ()
draw3dsegSolid col w = do
  let Trapezium(Pole(_,x1,y1f,y1c),Pole(_,x2,y2f,y2c)) = w

  let fstep :: Float = fromIntegral (y2f-y1f) / fromIntegral (x2-x1)
  let cstep :: Float = fromIntegral (y2c-y1c) / fromIntegral (x2-x1)

  sequence_ [ LineQ col (V2 x yf) (V2 x yc)
            | x <- [x1..x2]
            , let yf = y1f + floor (fromIntegral (x-x1) * fstep)
            , let yc = y1c + floor (fromIntegral (x-x1) * cstep)
            ]


closestSegs :: Level -> Vertex -> [Seg]
closestSegs level pos = do
  let Level{segs=allSegs} = level
  let tree = reifyTree level
  let subs = viewTreeFromPos pos tree
  subs >>= expandSS allSegs

expandSS :: [Seg] -> Subsector -> [Seg]
expandSS allSegs Subsector{first,count} =
  [ allSegs!!(fromIntegral i) | i <- [ first .. first+count-1 ] ]

viewTreeFromPos :: Vertex -> Tree -> [Subsector]
viewTreeFromPos playerPos = collectSS
  where
    collectSS :: Tree -> [Subsector]
    collectSS = \case
      Branch l n r -> do
        case onLeftSide playerPos n of
          True -> do collectSS l ++ collectSS r
          False -> do collectSS r ++ collectSS l
      Leaf ss ->
        [ss]

onLeftSide :: Vertex -> Node -> Bool
onLeftSide pos Node{start=partition,delta} = do
  cross (pos - partition) delta <= 0

cross :: V2 Int16 -> V2 Int16 -> Int -- TODO: aggh, Int16 lose precision!
cross (V2 x1 y1) (V2 x2 y2) =
  f x1*f y2 - f x2*f y1
  where f = fromIntegral

data Tree = Branch Tree Node Tree | Leaf Subsector

reifyTree :: Level -> Tree
reifyTree Level{subsectors,nodes} = tNode (nodes!!(length nodes - 1))
  where
    tNode :: Node -> Tree
    tNode n@Node{rightChildId,leftChildId} = Branch (tId leftChildId) n (tId rightChildId)
    tId :: Int16 -> Tree
    tId id = do
      if id >= 0 then tNode (nodes!! fromIntegral id) else do
        let ssId :: Word16 = (1 `shiftL` 15) + fromIntegral id
        let ss = subsectors!!(fromIntegral ssId)
        Leaf ss

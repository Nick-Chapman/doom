
module DrawMap -- TODO: rename Render
  ( draw, Views(..)
  ) where

import Data.Bits (shiftL)
import Data.Word (Word16)
import Colour (Colour,grey,red,magenta)
import Pic (Pic(..),V2(..))
import ProjectToScreen (POV(..),Trapezium(..),Pole(..),compTrapezium,visibleTrap)
import Wad (Wad(..),Level(..),Vertex,Node(..),Subsector(..),Seg(..),Linedef(..))

data Views = View2 | View3 | ViewBoth deriving Show

draw :: Views -> [Colour] -> Wad -> POV -> Pic ()
draw views randCols wad pov = do
  let Wad{level1=level} = wad
  let segs = closestSegs level pov
  -- TODO: Should peg random colours to segId, not the current nearness order
  let colouredSegs = zip randCols segs
  let csts :: [(Colour,Seg,Trapezium)] =
        [ (col,seg,trap)
        | (col,seg) <- colouredSegs
        , let trap = compTrapezium pov wad seg
        ]
  let v2 = draw2d wad pov csts
  let v3 = draw3d wad csts
  case views of View2 -> v2; View3 -> v3; ViewBoth -> do v2; v3

draw2d :: Wad -> POV -> [(Colour,Seg,Trapezium)] -> Pic ()
draw2d wad pov csts = do
  let Wad{level1=level} = wad
  let Level{vertexes} = level
  sequence_ [ do drawSeg vertexes col seg
            | (rcol,seg,trap) <- csts
            , let isVis = visibleTrap trap
            , let isPort = isPortal wad seg
            , let col = if isPort then grey else if isVis then rcol else red
            ]
  drawPlayer 45 pov

draw3d :: Wad -> [(Colour,Seg,Trapezium)] -> Pic ()
draw3d wad csts = do
  let vsolids =
        [ (col,trap)
        | (col,seg,trap) <- csts
        , not (isPortal wad seg) -- solid wall
        , visibleTrap trap -- in players persepective
        ]
  --Mes (show ("#vsolids",length vsolids))
  sequence_ [ do draw3dsegSolid col trap
            | (col,trap) <- reverse vsolids -- start further away; painter's alg
            ]

isPortal :: Wad -> Seg -> Bool
isPortal wad seg = do
  let Wad{level1=level} = wad
  let Level{linedefs} = level
  let Seg{linedefId} = seg
  let linedef = linedefs!!linedefId
  let Linedef{backSideId} = linedef
  backSideId /= -1

unquantize :: V2 Int -> V2 Float
unquantize = fmap fromIntegral

drawPlayer :: Int -> POV -> Pic ()
drawPlayer h_fov pov = do
  let POV{pos,angle} = pov
  Dot magenta (unquantize pos)
  let len = 3000
  let a = angle
  let a1 = a - h_fov
  let a2 = a + h_fov
  drawVec magenta pos a len
  drawVec magenta pos a1 len
  drawVec magenta pos a2 len

drawVec :: Colour -> Vertex -> Int -> Float -> Pic ()
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

draw3dsegSolid :: Colour -> Trapezium -> Pic ()
draw3dsegSolid col w = do
  let Trapezium(Pole(_,x1,y1f,y1c),Pole(_,x2,y2f,y2c)) = w
  let fstep :: Float = fromIntegral (y2f-y1f) / fromIntegral (x2-x1)
  let cstep :: Float = fromIntegral (y2c-y1c) / fromIntegral (x2-x1)
  sequence_ [ LineQ col (V2 x yf) (V2 x yc) -- TODO: use PointQ
            | x <- [x1..x2]
            , let yf = y1f + floor (fromIntegral (x-x1) * fstep)
            , let yc = y1c + floor (fromIntegral (x-x1) * cstep)
            ]

-- TODO: BSP code to sep file

closestSegs :: Level -> POV -> [Seg]
closestSegs level pov = do
  let Level{segs=allSegs} = level
  let tree = reifyTree level
  let subs = viewTreeFromPos pov tree
  subs >>= expandSS allSegs

expandSS :: [Seg] -> Subsector -> [Seg]
expandSS allSegs Subsector{first,count} =
  [ allSegs!!i | i <- [ first .. first+count-1 ] ]

viewTreeFromPos :: POV -> Tree -> [Subsector]
viewTreeFromPos pov = collectSS
  where
    POV{pos=playerPos} = pov
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

cross :: V2 Int -> V2 Int -> Int
cross (V2 x1 y1) (V2 x2 y2) = x1 * y2 - x2 * y1

data Tree = Branch Tree Node Tree | Leaf Subsector

reifyTree :: Level -> Tree
reifyTree Level{subsectors,nodes} = tNode (nodes!!(length nodes - 1))
  where
    tNode :: Node -> Tree
    tNode n@Node{rightChildId,leftChildId} = Branch (tId leftChildId) n (tId rightChildId)
    tId :: Int -> Tree
    tId id = do
      if id >= 0 then tNode (nodes!!id) else do
        let ssId :: Word16 = (1 `shiftL` 15) + fromIntegral id
        let ss = subsectors !! fromIntegral ssId
        Leaf ss

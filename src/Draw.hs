
module Draw
  ( everything, Views(..)
  ) where

import Data.Bits (shiftL)
import Data.Word (Word16)
import Colour (Colour,grey,red,magenta)
import Pic (Pic(..),V2(..))
import Project (POV(..),Trapezium(..),Pole(..),compTrapezium,visibleTrap)
import Wad (Wad(..),Level(..),Node(..),Subsector(..),Seg(..),Linedef(..))

data Views = View2 | View3 | ViewBoth deriving Show

everything :: Views -> [Colour] -> Wad -> POV -> Pic ()
everything views randCols wad pov = do
  let Wad{level1=level} = wad
  let segs = closestSegs level pov
  let colOfSeg Seg{segId} = randCols !! segId
  let csts :: [(Colour,Seg,Trapezium)] =
        [ (col,seg,trap)
        | seg <- segs
        , let col = colOfSeg seg
        , let trap = compTrapezium wad pov seg
        ]
  let v2 = draw2d pov csts
  let v3 = draw3d csts
  case views of View2 -> v2; View3 -> v3; ViewBoth -> do v2; v3

draw2d :: POV -> [(Colour,Seg,Trapezium)] -> Pic ()
draw2d pov csts = do
  sequence_ [ do drawSeg col seg
            | (rcol,seg,trap) <- csts
            , let isVis = visibleTrap trap
            , let isPort = isPortal seg
            , let col = if isPort then grey else if isVis then rcol else red
            ]
  drawPlayer 45 pov -- TODO: conf

draw3d :: [(Colour,Seg,Trapezium)] -> Pic ()
draw3d csts = do
  -- TODO: render portals
  let vsolids =
        [ (col,trap)
        | (col,seg,trap) <- csts
        , not (isPortal seg) -- solid wall
        , visibleTrap trap -- in players persepective
        ]
  --Mes (show ("#vsolids",length vsolids))
  sequence_ [ do draw3dsegSolid col trap
            | (col,trap) <- reverse vsolids -- start further away; painter's alg
            ]

isPortal :: Seg -> Bool
isPortal seg = do
  let Seg{linedef} = seg
  let Linedef{backSideId} = linedef
  backSideId /= -1

unquantize :: V2 Int -> V2 Float
unquantize = fmap fromIntegral

drawPlayer :: Float -> POV -> Pic ()
drawPlayer h_fov pov = do
  let POV{pos,angle} = pov
  Dot magenta pos
  let len = 3000
  let a = angle
  let a1 = a - h_fov
  let a2 = a + h_fov
  drawVec magenta pos a len
  drawVec magenta pos a1 len
  drawVec magenta pos a2 len

drawVec :: Colour -> V2 Float -> Float -> Float -> Pic ()
drawVec col pos angle len = do
  let r = radians angle
  let vec = V2 (len * cos r) (len * sin r)
  Line col pos (pos + vec)
  where
    radians n = n * pi / 180

drawSeg :: Colour -> Seg -> Pic ()
drawSeg col seg = do
  let Seg{start,end} = seg
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

onLeftSide :: V2 Float -> Node -> Bool
onLeftSide pos Node{start=partition,delta} = do
  cross (pos - unquantize partition) (unquantize delta) <= 0

cross :: V2 Float -> V2 Float -> Float
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

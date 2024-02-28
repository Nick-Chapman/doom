
module DrawMap
  ( draw
  ) where

import Pic (Pic(..),Colour,V2(..),grey,magenta,yellow,green,red,blue)
import Wad (Wad(..),Level(..),Thing(..),Linedef(..),Vertex,Int16
           ,Node(..),BB(..),Subsector(..))
import Data.Word (Word16)
import Data.Bits (shiftL)

draw :: Wad -> Pic ()
draw Wad{level1=level,player} = do
  let Level{linedefs,vertexes} = level
  drawLines linedefs
  let _ = mapM_ drawVertex vertexes
  drawPlayer 45 player
  drawTreeToPlayer (reifyTree level)
  where
    drawTreeToPlayer :: Tree -> Pic ()
    drawTreeToPlayer = \case
      Branch l _n r -> do
        drawNode _n
        if onBackSideForPlayer player _n
          then drawTreeToPlayer l -- back: GO left
          else drawTreeToPlayer r -- front: GO right
      Leaf ss ->
        drawSS ss

onBackSideForPlayer :: Thing -> Node -> Bool
onBackSideForPlayer Thing{pos=player} Node{start=partition,delta} = do
  cross (player - partition) delta <= 0

cross :: V2 Int16 -> V2 Int16 -> Int -- TODO: aggh, Int16 lose precision!
cross (V2 x1 y1) (V2 x2 y2) =
  f x1*f y2 - f x2*f y1
  where f = fromIntegral

drawLines :: [Linedef] -> Pic ()
drawLines linedefs = do
  sequence_ [ drawLinedef grey ld | ld <- linedefs ]
  pure ()

drawPlayer :: Int16 -> Thing -> Pic ()
drawPlayer h_fov Thing{pos,angle} = do
  Dot magenta (unquantize pos)
  let len = 1000
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

unquantize :: V2 Int16 -> V2 Float
unquantize = fmap fromIntegral

drawVertex :: Vertex -> Pic ()
drawVertex v = do
  Dot yellow (unquantize v)

drawLinedef :: Colour -> Linedef -> Pic ()
drawLinedef col Linedef{start,end} = do
  Line col (unquantize start) (unquantize end)

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

data Tree = Branch Tree Node Tree | Leaf Subsector

drawSS :: Subsector -> Pic ()
drawSS _ = do
  pure ()

drawNode :: Node -> Pic ()
drawNode Node{start,delta,rightBB,leftBB} = do
  Pause
  drawBB green rightBB
  drawBB red leftBB
  let end = start + delta
  Line blue (unquantize start) (unquantize end)
  pure ()

drawBB :: Colour -> BB -> Pic ()
drawBB col BB {top,bottom,left,right} = do
  let a = V2 left top
  let b = V2 right bottom
  Rect col (unquantize a) (unquantize b)

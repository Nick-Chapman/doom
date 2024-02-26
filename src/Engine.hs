
module Engine
  ( run
  ) where

import Wad (Wad(..),Level(..))

run :: Wad -> IO ()
run wad = do
  print "*Engine.run*"
  let Wad{dict,level1=Level{vertexes}} = wad
  --mapM_ print dict
  --mapM_ print vertexes
  print (length dict, length vertexes)

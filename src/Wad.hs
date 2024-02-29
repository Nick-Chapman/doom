
module Wad
  ( load, Wad(..), Level(..)
  , Thing(..), Linedef(..), Vertex, V2(..), Int16
  , Seg(..), Subsector(..), Node(..),BB(..)
  ) where

import Data.Bits (shiftL)
import Data.ByteString (ByteString,indexMaybe)
import Data.ByteString.Internal (w2c)
import Data.Int (Int16,Int32)
import Data.Word (Word8)
import Linear.V2 (V2(..))
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map

load :: FilePath -> IO Wad
load path = readWad <$> ByteString.readFile path

data Wad = Wad
  { identification :: String
  , numlumps :: Int32
  , infotableofs :: Int32
  , dict :: [Entry]
  , level1 :: Level
  , player :: Thing
  } deriving Show

data Entry = Entry
  { filepos :: Int32
  , size :: Int32
  , name :: String
  } deriving Show

data Level = Level
  { things :: [Thing]
  , linedefs :: [Linedef]
  , vertexes :: [Vertex]
  , segs :: [Seg]
  , subsectors :: [Subsector]
  , nodes :: [Node]
  } deriving Show

data Thing = Thing
  { pos :: V2 Int16
  , angle :: Int16
  -- TODO: 3 more fields
  } deriving Show

data Linedef = Linedef
  { start :: Vertex
  , end :: Vertex
  -- TODO: 3 more fields: flags, special-type, sector-tag
  , frontSideId :: Int
  , backSideId :: Int
  } deriving Show

data Sidedef = Sidedef {} deriving Show

type Vertex = V2 Int16 -- TODO: prefer Int!

data Seg = Seg -- TODO: explore using Int instead of Int16...
  { startId :: Int
  , endId :: Int
  , angle :: Int
  , linedefId :: Int16
  , direction :: Bool
  , offset :: Int16
  } deriving Show

data Subsector = Subsector
  { count :: Int16
  , first :: Int16
  } deriving Show

data BB = BB
  { top :: Int16
  , bottom :: Int16
  , left :: Int16
  , right :: Int16
  } deriving Show

data Node = Node
  { start :: Vertex
  , delta :: Vertex
  , rightBB :: BB
  , leftBB :: BB
  , rightChildId :: Int16
  , leftChildId :: Int16
  } deriving Show

readWad :: ByteString -> Wad
readWad bs = do
  let identification = readAscii bs 0 4
  let numlumps = readInt32 bs 4
  let infotableofs = readInt32 bs 8
  let dict = readDict bs (fromIntegral infotableofs) (fromIntegral numlumps)
  let getEntryIndex name = head [ i | (i,Entry{name=n}) <- zip [0..] dict, name==n ]
  let level1@Level{things} = readLevel bs dict (getEntryIndex "E1M1")
  let player = things!!0
  Wad { identification, numlumps, infotableofs, dict, level1, player }

readDict :: ByteString -> Offset -> Int -> [Entry]
readDict bs off n = do
  [ readEntry bs (off+16*i) | i <- [0..n-1] ]

readEntry :: ByteString -> Offset -> Entry
readEntry bs off = do
  let filepos = readInt32 bs off
  let size = readInt32 bs (off+4)
  let name = readAscii bs (off+8) 8
  Entry { filepos, size, name }

readLevel :: ByteString -> [Entry] -> Int -> Level
readLevel bs dict i = do
  let things = readThings bs (dict!!(i+1))
  let vertexes = readVertexes bs (dict!!(i+4))
  let m = Map.fromList (zip [0..] vertexes)
  let lookV n = maybe undefined id (Map.lookup n m)
  let linedefs = readLinedefs lookV bs (dict!!(i+2))
  -- 3:SIDEDEFS
  let segs = readSegs bs (dict!!(i+5))
  let subsectors = readSubsectors bs (dict!!(i+6))
  let nodes = readNodes bs (dict!!(i+7))
  -- 8:SECTORS, 9:REJECT, 10:BLOCKMAP
  Level { things, linedefs, vertexes, segs, subsectors, nodes }

readThings :: ByteString -> Entry -> [Thing]
readThings bs Entry{filepos,size,name} = do
  let nbytes = 10
  assertEq name "THINGS" $ do
  i <- [0.. size `div` nbytes - 1]
  let off = fromIntegral (filepos + nbytes * i)
  let x = readInt16 bs off
  let y = readInt16 bs (off+2)
  let angle = readInt16 bs (off+4)
  pure Thing { pos = V2 x y, angle }

readVertexes :: ByteString -> Entry -> [Vertex]
readVertexes bs Entry{filepos,size,name} = do
  let nbytes = 4
  assertEq name "VERTEXES" $ do
  i <- [0.. size `div` nbytes - 1]
  let off = fromIntegral (filepos + nbytes * i)
  let x = readInt16 bs off
  let y = readInt16 bs (off+2)
  pure (V2 x y)

readLinedefs :: (Int16 -> Vertex) -> ByteString -> Entry -> [Linedef]
readLinedefs lookV bs Entry{filepos,size,name} = do
  let nbytes = 14
  assertEq name "LINEDEFS" $ do
  i <- [0.. size `div` nbytes - 1]
  let off = fromIntegral (filepos + nbytes * i)
  let start = lookV $ readInt16 bs off
  let end = lookV $ readInt16 bs (off+2)
  -- 6 bytes for 3 other fields
  let frontSideId = readInt16' bs (off+10)
  let backSideId = readInt16' bs (off+12)
  pure Linedef { start, end, frontSideId, backSideId }

readSegs :: ByteString -> Entry -> [Seg]
readSegs bs Entry{filepos,size,name} = do
  let nbytes = 12
  assertEq name "SEGS" $ do
  i <- [0.. size `div` nbytes - 1]
  let off = fromIntegral (filepos + nbytes * i)
  let startId = fromIntegral $ readInt16 bs off
  let endId = readInt16' bs (off+2)
  let angle = readInt16' bs (off+4)
  let linedefId = readInt16 bs (off+6)
  let direction = readBool bs (off+8)
  let offset = readInt16 bs (off+10)
  pure Seg { startId, endId, angle, linedefId, direction, offset }

readSubsectors :: ByteString -> Entry -> [Subsector]
readSubsectors bs Entry{filepos,size,name} = do
  let nbytes = 4
  assertEq name "SSECTORS" $ do
  i <- [0.. size `div` nbytes - 1]
  let off = fromIntegral (filepos + nbytes * i)
  let count = readInt16 bs off
  let first = readInt16 bs (off+2)
  pure Subsector { count, first }

readNodes :: ByteString -> Entry -> [Node]
readNodes bs Entry{filepos,size,name} = do
  let nbytes = 28
  assertEq name "NODES" $ do
  i <- [0.. size `div` nbytes - 1]
  let off = fromIntegral (filepos + nbytes * i)
  let startX = readInt16 bs off
  let startY = readInt16 bs (off+2)
  let start = V2 startX startY
  let deltaX = readInt16 bs (off+4)
  let deltaY = readInt16 bs (off+6)
  let delta = V2 deltaX deltaY
  -- 16 bytes for bounding boxes
  let rightBB = readBB bs (off+8)
  let leftBB = readBB bs (off+16)
  let rightChildId = readInt16 bs (off+24)
  let leftChildId = readInt16 bs (off+26)
  pure Node { start, delta, rightBB, leftBB, rightChildId, leftChildId }

readBB :: ByteString -> Offset -> BB
readBB bs off = do
  let top = readInt16 bs off
  let bottom = readInt16 bs (off+2)
  let left = readInt16 bs (off+4)
  let right = readInt16 bs (off+6)
  BB { top, bottom, left, right }

readBool :: ByteString -> Offset -> Bool
readBool bs off =
  case readInt16 bs off of
    1 -> True
    0 -> False
    n -> error (show ("readBool",n))

type Offset = Int

(!) :: ByteString -> Offset -> Word8
(!) bs i = maybe err id (indexMaybe bs i)
  where err = error (show ("ByteString-!",i))

readAscii :: ByteString -> Offset -> Int -> String
readAscii bs off n =
  takeWhile (/='\0') [ w2c (bs!i) | i <- take n [off..] ]

readInt32 :: ByteString -> Offset -> Int32
readInt32 bs off =
  sum [ fromIntegral (bs!(off+i)) `shiftL` (8*i) | i <- take 4 [0..] ]

readInt16' :: ByteString -> Offset -> Int -- TODO: become standard
readInt16' bs off = fromIntegral (readInt16 bs off)

readInt16 :: ByteString -> Offset -> Int16
readInt16 bs off =
  sum [ fromIntegral (bs!(off+i)) `shiftL` (8*i) | i <- take 2 [0..] ]

assertEq :: (Show a, Eq a) =>  a -> a -> x -> x
assertEq a b x =
  if a==b then x else error (show ("assertEq",a,b))

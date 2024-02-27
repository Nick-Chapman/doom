
module Wad
  ( load, Wad(..), Level(..)
  , Thing(..), Linedef(..), Vertex, V2(..), Int16
  ) where

import Data.Bits (shiftL)
import Data.ByteString (ByteString,indexMaybe)
import Data.ByteString.Internal (w2c)
import Data.Int (Int16,Int32)
import Data.Word (Word8)
import Linear.V2 (V2(..))
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map

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
  } deriving Show

data Thing = Thing
  { pos :: V2 Int16
  , angle :: Int16
  -- TODO: 3 more fields
  } deriving Show

data Linedef = Linedef
  { start :: Vertex
  , end :: Vertex
  -- TODO: 5 more fields
  } deriving Show

type Vertex = V2 Int16

load :: FilePath -> IO Wad
load path = readWad <$> ByteString.readFile path

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
  Level { things, linedefs, vertexes }

readThings :: ByteString -> Entry -> [Thing]
readThings bs Entry{filepos,size,name} = do
  let nbytes = 10
  assertEq name "THINGS" $ do
  i <- [0.. size `div` nbytes - 1]
  pure (readThing bs (fromIntegral (filepos + nbytes * i)))

readThing :: ByteString -> Offset -> Thing
readThing bs off = do
  let x = readInt16 bs off
  let y = readInt16 bs (off+2)
  let angle = readInt16 bs (off+4)
  Thing { pos = V2 x y, angle }

readVertexes :: ByteString -> Entry -> [Vertex]
readVertexes bs Entry{filepos,size,name} = do
  let nbytes = 4
  assertEq name "VERTEXES" $ do
  i <- [0.. size `div` nbytes - 1]
  pure (readVertex bs (fromIntegral (filepos + nbytes * i)))

readLinedefs :: (Int16 -> Vertex) -> ByteString -> Entry -> [Linedef]
readLinedefs lookV bs Entry{filepos,size,name} = do
  let nbytes = 14
  assertEq name "LINEDEFS" $ do
  i <- [0.. size `div` nbytes - 1]
  pure (readLinedef lookV bs (fromIntegral (filepos + nbytes * i)))

readLinedef :: (Int16 -> Vertex) -> ByteString -> Offset -> Linedef
readLinedef lookV bs off = do
  let start = lookV $ readInt16 bs off
  let end = lookV $ readInt16 bs (off+2)
  Linedef { start, end }

readVertex :: ByteString -> Offset -> Vertex
readVertex bs off = do
  let x = readInt16 bs off
  let y = readInt16 bs (off+2)
  V2 x y

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

readInt16 :: ByteString -> Offset -> Int16
readInt16 bs off =
  sum [ fromIntegral (bs!(off+i)) `shiftL` (8*i) | i <- take 2 [0..] ]

assertEq :: (Show a, Eq a) =>  a -> a -> x -> x
assertEq a b x =
  if a==b then x else error (show ("assertEq",a,b))

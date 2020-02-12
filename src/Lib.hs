module Lib where

import Control.Monad
import Data.Bool
import Data.Bits
import System.HID
import Data.ByteString (ByteString, unpack)
import Data.Word

data Button
  = Shift
  | Scale
  | Arp
  | Undo
  | Quantize
  | Ideas
  | Loop
  | Metro
  | Tempo
  | Play
  | Rec
  | Stop
  | PresetUp
  | PresetDown
  | PageLeft
  | PageRight
  | Browser
  | Plugin
  | Track
  | OctaveDown
  | OctaveUp
  | Unknown1
  | Unknown2
  | Unknown3
  | Unknown4
  | Dial1Press
  | Dial2Press
  | Dial3Press
  | Dial4Press
  | Dial5Press
  | Dial6Press
  | Dial7Press
  | Dial8Press
  | VolPress
  | Unknown5
  | Unknown6
  | Unknown7
  | Unknown8
  | Unknown9
  | Unknown10
  deriving (Eq, Ord, Show, Enum, Bounded)


lookupButton :: [Word8] -> Button -> Bool
lookupButton ws b =
  let ival = fromEnum b
      bit = ival `mod` 8
      byte = ival `div` 8
   in testBit (ws !! byte) bit


main :: IO ()
main = do
  Just dev <- vendorProductSerialDevice 0x17cc 0x1860 Nothing
  forever $ do
    Just bs <- readInputReport dev
    let bytes = drop 1 $ unpack bs
    print $ filter (lookupButton bytes) [Shift ..]


unbitWord8 :: Word8 -> [Bool]
unbitWord8 w = reverse $ fmap (testBit w) [0..7]


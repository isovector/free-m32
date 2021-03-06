{-# LANGUAGE NumDecimals #-}

module Lib where

import Data.Foldable
import Data.Char
import Debug.Trace
import Control.Concurrent
import Control.Monad
import Data.Bool
import Data.Bits
import System.HID
import Data.ByteString (ByteString, unpack)
import Data.Word
import Data.Maybe
import Sound.PortMidi
import Data.Either
import Data.List
import System.Process (spawnProcess)

data RawButton
  = RawShift
  | RawScale
  | RawArp
  | RawUndo
  | RawQuantize
  | RawIdeas
  | RawLoop
  | RawMetro
  | RawTempo
  | RawPlay
  | RawRec
  | RawStop
  | RawPresetUp
  | RawPresetDown
  | RawPageLeft
  | RawPageRight
  | RawBrowser
  | RawPlugin
  | RawTrack
  | RawOctaveDown
  | RawOctaveUp
  | RawUnknown1
  | RawUnknown2
  | RawUnknown3
  | RawUnknown4
  | RawDial1Touch
  | RawDial2Touch
  | RawDial3Touch
  | RawDial4Touch
  | RawDial5Touch
  | RawDial6Touch
  | RawDial7Touch
  | RawDial8Touch
  | RawVolPress
  | RawUnknown5
  | RawUnknown6
  | RawUnknown7
  | RawUnknown8
  | RawUnknown9
  | RawUnknown10
  deriving (Eq, Ord, Show, Enum, Bounded)

data LogicalButton
  = Scale
  | Edit1
  | Arp
  | Edit2
  | Undo
  | Redo
  | Quantize
  | Auto
  | Ideas
  | Loop
  | Metro
  | Tempo
  | Play
  | Restart
  | Rec
  | CountIn
  | Stop
  | Clear
  | PresetUp
  | PresetDown
  | PageLeft
  | PageRight
  | Browser
  | Plugin
  | MIDI
  | Track
  | Instance
  | OctaveDown
  | FixedVelocity
  | OctaveUp
  | KeyMode
  -- | Dial1Touch
  -- | Dial2Touch
  -- | Dial3Touch
  -- | Dial4Touch
  -- | Dial5Touch
  -- | Dial6Touch
  -- | Dial7Touch
  -- | Dial8Touch
  | VolPress
  deriving (Eq, Ord, Show, Enum, Bounded)

rawToLogical :: Bool -> RawButton -> Maybe LogicalButton
rawToLogical _     RawShift      = Nothing
rawToLogical False RawScale      = Just Scale
rawToLogical True  RawScale      = Just Edit1
rawToLogical False RawArp        = Just Arp
rawToLogical True  RawArp        = Just Edit2
rawToLogical False RawUndo       = Just Undo
rawToLogical True  RawUndo       = Just Redo
rawToLogical False RawQuantize   = Just Quantize
rawToLogical True  RawQuantize   = Just Auto
rawToLogical _     RawIdeas      = Just Ideas
rawToLogical _     RawLoop       = Just Loop
rawToLogical _     RawMetro      = Just Metro
rawToLogical _     RawTempo      = Just Tempo
rawToLogical False RawPlay       = Just Play
rawToLogical True  RawPlay       = Just Restart
rawToLogical False RawRec        = Just Rec
rawToLogical True  RawRec        = Just CountIn
rawToLogical False RawStop       = Just Stop
rawToLogical True  RawStop       = Just Clear
rawToLogical _     RawPresetUp   = Just PresetUp
rawToLogical _     RawPresetDown = Just PresetDown
rawToLogical _     RawPageLeft   = Just PageLeft
rawToLogical _     RawPageRight  = Just PageRight
rawToLogical _     RawBrowser    = Just Browser
rawToLogical False RawPlugin     = Just Plugin
rawToLogical True  RawPlugin     = Just MIDI
rawToLogical False RawTrack      = Just Track
rawToLogical True  RawTrack      = Just Instance
rawToLogical False RawOctaveDown = Just OctaveDown
rawToLogical True  RawOctaveDown = Just FixedVelocity
rawToLogical False RawOctaveUp   = Just OctaveUp
rawToLogical True  RawOctaveUp   = Just KeyMode
rawToLogical _     RawUnknown1   = Nothing
rawToLogical _     RawUnknown2   = Nothing
rawToLogical _     RawUnknown3   = Nothing
rawToLogical _     RawUnknown4   = Nothing
rawToLogical _     RawDial1Touch = Nothing -- Just Dial1Touch
rawToLogical _     RawDial2Touch = Nothing -- Just Dial2Touch
rawToLogical _     RawDial3Touch = Nothing -- Just Dial3Touch
rawToLogical _     RawDial4Touch = Nothing -- Just Dial4Touch
rawToLogical _     RawDial5Touch = Nothing -- Just Dial5Touch
rawToLogical _     RawDial6Touch = Nothing -- Just Dial6Touch
rawToLogical _     RawDial7Touch = Nothing -- Just Dial7Touch
rawToLogical _     RawDial8Touch = Nothing -- Just Dial8Touch
rawToLogical _     RawVolPress   = Just VolPress
rawToLogical _     RawUnknown5   = Nothing
rawToLogical _     RawUnknown6   = Nothing
rawToLogical _     RawUnknown7   = Nothing
rawToLogical _     RawUnknown8   = Nothing
rawToLogical _     RawUnknown9   = Nothing
rawToLogical _     RawUnknown10  = Nothing


lookupRawButton :: [Word8] -> RawButton -> Bool
lookupRawButton ws b =
  let ival = fromEnum b
      bit = ival `mod` 8
      byte = ival `div` 8
   in testBit (ws !! byte) bit


data M32Report = M32Report
  { m32Raw      :: [RawButton]
  , m32Logical  :: [LogicalButton]
  , m32Vol      :: Word8
  , m32Keyshift :: Word8
  } deriving (Eq, Ord, Show)


getReport :: Device -> IO M32Report
getReport dev = do
  Just bs <- readInputReport dev
  let bytes      = drop 1 $ unpack bs
      rawbuttons = filter (lookupRawButton bytes) [RawShift ..]
      shifted    = elem RawShift rawbuttons
      logical    = mapMaybe (rawToLogical shifted) rawbuttons
      volume     = (bytes !! 23) .&. 15
      keyshift   = bytes !! 36
  pure $ M32Report rawbuttons logical volume keyshift


getNotableDevices :: IO [DeviceID]
getNotableDevices = do
  num_devs <- countDevices
  devs <- traverse getDeviceInfo [0 .. num_devs - 1]
  pure $ do
    (dev, idx) <- zip devs [0..]
    traceM $ show dev
    guard $ isPrefixOf "KOMPLETE KONTROL M32 MIDI" $ name dev
    guard $ output dev
    pure idx


main :: IO ()
main = do
  Just dev <- vendorProductSerialDevice 0x17cc 0x1860 Nothing
  forever $ do
    rep <- getReport dev
    for_ (m32Logical rep) $ spawnProcess "xdotool" . ("type" :) . pure . dumpChar


dumpChar :: LogicalButton -> String
dumpChar = pure . toUpper . chr . (+ fromEnum 'A') . fromEnum


unbitWord8 :: Word8 -> [Bool]
unbitWord8 w = reverse $ fmap (testBit w) [0..7]


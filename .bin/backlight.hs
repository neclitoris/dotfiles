#!/usr/bin/env stack
-- stack script --snapshot lts-18.10 --optimize

import Control.Monad
import System.IO
import System.Environment

maxBrightness :: IO Int
maxBrightness =
  withFile
    "/sys/class/backlight/intel_backlight/max_brightness"
    ReadMode
    (readIO <=< hGetContents)

readBrightness :: IO Int
readBrightness =
  withFile
    "/sys/class/backlight/intel_backlight/brightness"
    ReadMode
    (readIO <=< hGetContents)

writeBrightness :: Int -> IO ()
writeBrightness n =
  withFile
    "/sys/class/backlight/intel_backlight/brightness"
    WriteMode
    (`hPrint` n)

snap :: Int -> Int -> Int
snap max val
  | val > max = max
  | val < 0 = 0
  | otherwise = val

main' ["query"] = do
  max <- maxBrightness
  cur <- readBrightness
  print $ cur * 100 `div` max
main' ["update", num] = do
  max <- maxBrightness
  cur <- readBrightness
  let newVal = snap max $ case head num of
                 '+' -> cur + read (tail num) * max `div` 100
                 '-' -> cur - read (tail num) * max `div` 100
                 _   -> read num * max `div` 100
  writeBrightness newVal
  print $ newVal * 100 `div` max
main' _ = putStrLn "invalid usage"

main = getArgs >>= main'

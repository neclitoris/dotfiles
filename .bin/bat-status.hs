#!/usr/bin/env stack
-- stack script --snapshot lts-18.10 --package turtle --package filepath --package process --package time
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Control.Monad
import Data.Ratio
import Data.Time
import Data.Time.Clock.POSIX
import System.Environment
import System.IO
import System.FilePath
import System.Process
import Turtle (NominalDiffTime)
import Turtle.Prelude

width :: Int -> String -> String
width n s
  | length s < n = replicate (n - length s) ' ' ++ s
  | otherwise    = s

dir = "/sys/class/power_supply"

main = do
  load <- withFile (dir </> "BAT0/capacity") ReadMode (readIO <=< hGetContents) :: IO Int
  ac <- withFile (dir </> "AC/online") ReadMode (readIO <=< hGetContents) :: IO Int
  home <- getEnv "HOME"
  let icon_cat = if ac == 0
                    then "off/battery_off_"
                    else "on/battery_on_"
  let icon_num = truncate (fromIntegral load * (8 % 100))
  let icon_file = home </> "util/xpm-status-icons/icons/battery/" </> icon_cat <> show icon_num <> ".xpm"
  putStrLn ("<icon=" <> icon_file <> "/> " <> width 3 (show load) <> "%")
  when (ac == 0 && load <= 20) $ do
    time <- (accessTime <$> stat "/tmp/bat-status") `catch` (\(_ :: IOError) -> return 0)
    now <- utcTimeToPOSIXSeconds <$> date
    when (now - time >= 60) $ do
      act <- readProcess "dunstify"
        [ "-i", icon_file
        , "-a", "bat-status"
        , "-h", "string:x-canonical-private-synchronous:bat-status"
        , "-A", "hide,hide"
        , "Battery low (" <> show load <> "%)"]
        ""
      when (lines act /= ["1"]) $ touch "/tmp/bat-status"

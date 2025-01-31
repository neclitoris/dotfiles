#!/usr/bin/env stack
-- stack script --snapshot lts-18.10 --package turtle --package filepath --package process  --optimize

import Data.Char
import Data.Ratio
import System.Environment
import System.FilePath
import System.Process

width :: Int -> String -> String
width n s
  | length s < n = replicate (n - length s) ' ' ++ s
  | otherwise    = s

readline = readIO . takeWhile isDigit

main = do
  loads <- words <$> readProcess "neofetch" ["memory"] ""
  used <- readline (loads !! 1) :: IO Int
  total <- readline (loads !! 3) :: IO Int
  home <- getEnv "HOME"
  let load = used % total
  let icon_num = truncate (load * 8)
  let icon_file = home </> "util/xpm-status-icons/icons/ram/ram_" <> show icon_num <> ".xpm"
  putStrLn ("<icon=" <> icon_file <> "/> " <> width 3 (show $ truncate $ load * 100) <> "%")

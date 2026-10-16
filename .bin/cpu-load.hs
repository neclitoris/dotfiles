#!/usr/bin/env stack
-- stack script --snapshot lts-18.10 --package turtle --package filepath --package process  --optimize

import Data.Ratio
import System.Environment
import System.FilePath
import System.IO
import System.Process

width :: Int -> String -> String
width n s
  | length s < n = replicate (n - length s) ' ' ++ s
  | otherwise    = s

main = do
  idleStr <- readCreateProcess (shell "top -bn1 | grep -o -P \"[0-9]+\\.[0-9]+(?= id)\" | head -n 1") ""
  load <- (100-) . truncate <$> readIO idleStr
  home <- getEnv "HOME"
  let icon_num = truncate (fromIntegral load / (100 % 8))
  let icon_file = home </> "util/xpm-status-icons/icons/cpu/cpu_" <> show icon_num <> ".xpm"
  putStrLn ("<icon=" <> icon_file <> "/> " <> width 3 (show load) <> "%")

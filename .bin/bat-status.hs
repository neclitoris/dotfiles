#!/home/neclitoris/.bin/runghc-cached.sh

import Control.Monad
import Data.Ratio
import System.Environment
import System.IO
import System.FilePath

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

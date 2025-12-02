import Data.String.Interpolate ( __i )
import Data.List ( intercalate )
import Xmobar

main :: IO ()
main = xmobar config

-- Dark
base0, base1, base2, base3 :: String
base0 = "#657b83"
base1 = "#586e75"
base2 = "#073642"
base3 = "#002b36"

-- -- Light
base00, base01, base02, base03 :: String
base00 = "#839496"
base01 = "#93a1a1"
base02 = "#eee8d5"
base03 = "#fdf6e3"

yellow, orange, red, magenta, violet, blue, cyan, green :: String
yellow = "#b58900"
orange = "#cb4b16"
red = "#dc322f"
magenta = "#d33682"
violet = "#6c71c4"
blue = "#268bd2"
cyan = "#2aa198"
green = "#859900"

-- 

mkfonts list = intercalate "," $ map fst list ++ reverse (map snd list)

config :: Config
config = defaultConfig
        { font = mkfonts [ ("Source Code Pro for Powerline", "Regular 14")
                         , ("Noto Sans CJK SC", "14")
                         , ("Noto Sans CJK JP", "14")
                         , ("Noto Sans CJK KR", "14")
                         ]
        , position = Static { xpos = 8, ypos = 8, width = 1904, height = 22 }
        , bgColor = base2
        , fgColor = base0
        , alpha = 255
        , textOffset = 0
        , commands = [ Run $ Com "cpu-load.hs" [] "cpu" 10
                     , Run $ Com "mem-load.hs" [] "memory" 10
                     , Run $ Com "bat-status.hs" [] "battery" 10
                     , Run $ Com "updates" [] "updates" 36000
                     , Run $ Swap [] 10
                     , Run $ Date "%a %F %H:%M:%S" "date" 10
                     , Run UnsafeXMonadLog
                     , Run $ Kbd [("us", "US"), ("ru", "RU")]
                     , Run $ Com "xmobar-stalonetray-pad" [] "traypad" 10
                     ]
        , sepChar = "%"
        , alignSep = "}{"
        , iconRoot = "/home/neclitoris/image/icons/"
        , template = (mconcat . lines)
          [__i|<fc=#{base02},#{base00}> </fc>%UnsafeXMonadLog% }{
             %cpu%
              %memory%
              %battery%
             <fc=#{base0}></fc>
            <fc=#{red},#{base0}> %kbd% </fc>
            <fc=#{base00},#{base0}></fc>
            <fc=#{base02},#{base00}>
             %date%
             
             <action=`alacritty -e zsh -c 'checkupdates | vim -R -'` button=1>
            <action=`alacritty -e zsh -c 'yay -Syu; echo "Press any key..."; read -k'` button=3>
            %updates%
            </action>
            </action>
             %traypad%
             </fc>|]
        }

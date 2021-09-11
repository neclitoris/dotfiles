import Data.String.Interpolate ( __i )
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

config :: Config
config = defaultConfig
        { font = [__i|xft:Source Code Pro for Powerline:pixelsize=16:antialiase=true:Regular ,FontAwesome:pixelsize=16
                    ,Noto Sans CJK SC:pixelsize=16:antialiase=true
                    ,Noto Sans CJK JP:pixelsize=16:antialiase=true
                    ,Noto Sans CJK KR:pixelsize=16:antialiase=true|]
        , additionalFonts = ["xft:Source Code Pro for Powerline:pixelsize=18:antialiase=true:Regular"]
        , position = Static { xpos = 8, ypos = 8, width = 1904, height = 22 }
        , bgColor = base2
        , fgColor = base0
        , alpha = 255
        , textOffset = 16
        , textOffsets = [17]
        , commands = [ Run $ Com "cpu-load.hs" [] "cpu" 10
                     , Run $ Com "mem-load.hs" [] "memory" 10
                     , Run $ Com "bat-status.hs" [] "battery" 10
                     , Run $ Com "updates" [] "updates" 36000
                     , Run $ Swap [] 10
                     , Run $ Date "%a %F %H:%M:%S" "date" 10
                     , Run UnsafeStdinReader
                     , Run $ Kbd [("us", "US"), ("ru", "RU")]
                     , Run $ Com "xmobar-stalonetray-pad" [] "traypad" 10
                     ]
        , sepChar = "%"
        , alignSep = "}{"
        , iconRoot = "/home/neclitoris/image/icons/"
        , template = (mconcat . lines)
          [__i|<fc=#{base02},#{base00}> </fc>%UnsafeStdinReader% }{
            <fn=1></fn> %cpu%
             <fn=1></fn> %memory%
             <fn=1></fn> %battery%
             <fn=1><fc=#{base0}></fc></fn>
            <fc=#{red},#{base0}> %kbd% </fc>
            <fn=1><fc=#{base00},#{base0}></fc></fn>
            <fc=#{base02},#{base00}>
            <fn=1></fn> %date%
             <fn=1></fn>
             <action=`alacritty -e zsh -c 'checkupdates | vim -R -'` button=1>
            <action=`alacritty -e zsh -c 'yay -Syu'` button=3>
            %updates%
            </action>
            </action>
             <fn=1></fn>%traypad%
             </fc>|]
        }

import           Control.Exception
import           Control.Monad
import           Data.Default
import qualified Data.List                     as L
import qualified Data.Map                      as M
import           Data.Tree
import           Data.Word
import           Numeric
import           Text.Printf
import           System.Directory
import           System.Exit
import           System.FilePath.Posix
import qualified System.Process                as P
import           System.IO

import           XMonad
import qualified XMonad.Actions.TreeSelect     as TS
import           XMonad.Config.Gnome
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Layout.Hidden
import           XMonad.Prompt                 as XP
import           XMonad.Prompt.Shell           as XP
import           XMonad.Prompt.XMonad          as XP
import qualified XMonad.StackSet               as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run

-- Solarized colorscheme

-- Dark
base0, base1, base2, base3 :: Word64
base0 = 0xff657b83
base1 = 0xff586e75
base2 = 0xff073642
base3 = 0xff002b36

-- Light
base00, base01, base02, base03 :: Word64
base00 = 0xff839496
base01 = 0xff93a1a1
base02 = 0xffeee8d5
base03 = 0xfffdf6e3

yellow, orange, red, magenta, violet, blue, cyan, green :: Word64
yellow = 0xffb58900
orange = 0xffcb4b16
red = 0xffdc322f
magenta = 0xffd33682
violet = 0xff6c71c4
blue = 0xff268bd2
cyan = 0xff2aa198
green = 0xff859900

colorToStr :: Word64 -> String
colorToStr = ('#' :) . drop 2 . (`showHex` "")

terminalEmulator :: String
terminalEmulator = "alacritty"

data AllFloats = AllFloats deriving (Read, Show)

instance SetsAmbiguous AllFloats where
    hiddens _ wset _ _ _ = M.keys $ W.floating wset

myManageHook :: ManageHook
myManageHook = composeAll
    [ title =? "Media viewer" --> doFloat
    , className =? "Xmessage" --> doFloat
    , isFullscreen --> doFullFloat
    , manageDocks
    ]

screenLocker :: String
screenLocker = "slock"

getTmuxSessions :: IO [String]
getTmuxSessions = do
    str <- runProcessWithInput "tmux" ["list-sessions"] "" >>= (\s -> if null s then retry else return s)
    return $ map (takeWhile (/= ':')) $ lines str
  where
    retry :: IO String
    retry = runProcessWithInput
                    "tmux"
                    [ "start"
                    , ";"
                    , "run-shell"
                    , "~/.tmux/plugins/tmux-resurrect/scripts/restore.sh"
                    , ";"
                    , "list-sessions"
                    ]
                    ""

myXPConfig :: XP.XPConfig
myXPConfig = def { XP.font = "xft:Source Code Pro for Powerline:size=16"
                 , XP.bgColor = colorToStr base2
                 , XP.fgColor = colorToStr base0
                 , XP.bgHLight = colorToStr base00
                 , XP.fgHLight = colorToStr base03
                 , XP.borderColor = colorToStr base2
                 , XP.position = Top
                 , XP.height = 22
                 , XP.maxComplRows = Just 1
                 }

myTSConfig :: TS.TSConfig a
myTSConfig = TS.tsDefaultConfig { TS.ts_font = "xft:Source Code Pro for Powerline:size=12"
                                        , TS.ts_background = base2 - 0x3f000000
                                        , TS.ts_highlight  = (base03, base00)
                                        , TS.ts_node       = (base0, base2)
                                        , TS.ts_nodealt    = (base0, base2)
                                        , TS.ts_extra      = base02
                                        }

desktop :: IO [FilePath]
desktop = do
    home <- getHomeDirectory
    let comps = home
    let desk = comps </> "desktop"
    L.sort . fmap (desk </>) . filter ((== ".desktop") . takeExtension) <$> listDirectory desk

data TmuxRenamePrompt = TmuxRenamePrompt deriving (Read, Show)

instance XP.XPrompt TmuxRenamePrompt where
    showXPrompt _ = "Enter new name: "

myTreeSelect :: X ()
myTreeSelect = do
    tmuxSessions <- io getTmuxSessions
    desktopFiles <- io desktop
    let tree =  [ Node (TS.TSNode "quicklaunch" "some regularly used apps" (return ()))
                     (map (\(name, com) -> Node (TS.TSNode name "" (spawn com)) []) commonApps)
                , Node (TS.TSNode "desktop" "" (return ()))
                     (map (\file -> Node (TS.TSNode (desktopName file) "" (spawn ("dex " ++ file))) []) desktopFiles)
                , Node (TS.TSNode "tmux" "current tmux sessions" (return ()))
                     (map (\s -> Node (TS.TSNode s "" (attachSession s)) (manipSession s)) tmuxSessions)
                , Node (TS.TSNode "power" "" (return ()))
                     (map (\(name, com) -> Node (TS.TSNode name "" (spawn com)) []) powerComs)
                ]

    TS.treeselectAction myTSConfig tree
  where
    desktopName = dropExtension . takeFileName
    -- quicklaunch tab
    commonApps =
        [ ("firefox" , "firefox")
        , ("telegram", "telegram-desktop")
        , ("discord" , "discord")
        , ("steam"   , "steam")
        , ("gimp"    , "gimp")
        ]
    -- power tab
    powerComs =
        [ ("shutdown"   , "loginctl poweroff")
        , ("reboot"     , "loginctl reboot")
        , ("suspend"    , printf "loginctl suspend && %s" screenLocker)
        , ("lock screen", screenLocker)
        ]
    -- tmux tab
    manipSession s =
        [ Node (TS.TSNode "attach" "" (attachSession s)) []
        , Node (TS.TSNode "rename" "" (renameSession s)) []
        , Node (TS.TSNode "kill" "" (killSession s))     []
        ]
    attachSession s = runInTerm "" (printf "'tmux attach-session -t %s'" s)
    killSession s = spawn (printf "tmux kill-session -t %s" s)
    renameSession s = XP.mkXPrompt TmuxRenamePrompt myXPConfig (XP.mkComplFunFromList []) (spawn . printf "tmux rename-session -t %s %s" s)

myKeymap :: [(String, X ())]
myKeymap =
    [ ("M-h", sendMessage Shrink)
    , ("M-l", sendMessage Expand)
    , ("C-M1-l"    , spawn screenLocker)
    , ("<Print>"   , spawn "maim $(date +\"$HOME/image/Screenshots/%FT%T.png\")")
    , ("M1-<Print>", spawn "maim -i $(xdotool getactivewindow) $(date +\"$HOME/image/Screenshots/%FT%T.png\")")
    , ( "S-<Print>"
      , spawn "maim -s $(date +\"$HOME/image/Screenshots/%FT%T.png\")"
      )
    , ( "C-<Print>"
      , spawn "maim | tee /tmp/img.png | xclip -target image/png -sel clipboard"
      )
    , ( "C-M1-<Print>"
      , spawn "maim -i $(xdotool getactivewindow) | tee /tmp/img.png | xclip -target image/png -sel clipboard"
      )
    , ( "C-S-<Print>"
      , spawn "maim -s | tee /tmp/img.png | xclip -target image/png -sel clipboard"
      )
    , ( "M-f"
      , withFocused (flip tileWindow $ Rectangle 0 0 1920 1080)
      )
    , ("M-S-t", myTreeSelect)
    , ("M-p", XP.shellPrompt myXPConfig)
    , ("M-S-x", XP.xmonadPrompt myXPConfig)
    , ("C-M1-h", withFocused hideWindow)
    , ("C-M1-p", popOldestHiddenWindow)
    , ("<XF86AudioRaiseVolume>" , spawn "amixer set Master 3%+")
    , ("<XF86AudioLowerVolume>" , spawn "amixer set Master 3%-")
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<XF86MonBrightnessUp>", spawn "bright Up")
    , ("<XF86MonBrightnessDown>", spawn "bright Down")
    ]

myXmobar :: Handle -> X ()
myXmobar xmproc = dynamicLogWithPP xmobarPP
    { ppOutput  = hPutStrLn xmproc . xmobarColor "" (colorToStr base00)
    , ppTitle   = xmobarColor (colorToStr base0) ""
                  . ((  "<fn=1>"
                     ++ xmobarColor (colorToStr base00) (colorToStr base0) "\xe0b0"
                     ++ xmobarColor (colorToStr base0) "" "\xe0b0"
                     ++ "</fn> "
                     ) ++
                    )
                  . shorten 50
    , ppLayout  = xmobarColor (colorToStr base02) (colorToStr base00) . wrap " " ""
    , ppHidden  = xmobarColor (colorToStr base1) (colorToStr base00)
                      . (\s -> xmobarAction ("xdotool key super+" ++ s) "1" s)
    , ppCurrent = xmobarColor (colorToStr base02) (colorToStr base00)
    , ppSep     = xmobarColor (colorToStr base02) (colorToStr base00) " <fn=1>\xe0b1</fn>"
    }

myLayoutHook =
    hiddenWindows
        $   lessBorders OnlyScreenFloat
        $   avoidStruts
        $   spacingRaw False (Border 1 5 5 5) True (Border 3 3 3 3) True
        $   tall ||| Mirror tall ||| Full ||| spiral (6 / 7)
    where tall = Tall 1 (3/100) (1/2)

myConfig xmproc = ewmh $ docks $ def
    { modMask            = mod4Mask
    , terminal           = terminalEmulator
    , layoutHook         = myLayoutHook
    , logHook            = myXmobar xmproc
    , manageHook         = myManageHook <+> def
    , normalBorderColor  = colorToStr base2
    , focusedBorderColor = colorToStr base2
    } `additionalKeysP` myKeymap

main :: IO ()
main = do
    installSignalHandlers
    xmproc <- spawnPipe "xmobar $HOME/.xmonad/xmobarrc"
    xmonad $ myConfig xmproc

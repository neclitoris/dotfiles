{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable, FunctionalDependencies #-}
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
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.Actions.TreeSelect     as TS
import           XMonad.Config.Gnome
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
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
    [ title =? "Media viewer" --> doFullFloat
    , className =? "Xmessage" --> doFloat
    , manageDocks
    ]

screenLocker :: String
screenLocker = "lock"

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
myXPConfig = def { XP.font = "xft:Source Code Pro for Powerline:pixelsize=16:antialiase=true"
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
myTSConfig = TS.tsDefaultConfig { TS.ts_font = "xft:Source Code Pro for Powerline:pixelsize=16:antialiase=true"
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

data NoPrompt = NoPrompt

instance XP.XPrompt NoPrompt where
  showXPrompt _ = ""

data OverridePrompt p
  = OverridePrompt
    { oPrompt :: p
    , oLine :: String }

instance XP.XPrompt p => XP.XPrompt (OverridePrompt p) where
  showXPrompt = oLine
  nextCompletion = nextCompletion . oPrompt
  commandToComplete = commandToComplete . oPrompt
  completionToCommand = completionToCommand . oPrompt
  completionFunction = completionFunction . oPrompt
  modeAction = modeAction . oPrompt

namedShellPrompt = OverridePrompt XP.Shell
namedPrompt = OverridePrompt NoPrompt

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
        , ("steam"   , "prime-run steam")
        , ("gimp"    , "prime-run gimp")
        ]
    -- power tab
    powerComs =
        [ ("shutdown"   , "loginctl poweroff")
        , ("reboot"     , "loginctl reboot")
        , ("suspend"    , printf "(%s&) && sleep 1 && loginctl suspend" screenLocker)
        , ("lock screen", screenLocker)
        ]
    -- tmux tab
    manipSession s =
        [ Node (TS.TSNode "attach" "" (attachSession s)) []
        , Node (TS.TSNode "rename" "" (renameSession s)) []
        , Node (TS.TSNode "kill" "" (killSession s))     []
        ]
    attachSession s = runInTerm "" (printf "tmux attach-session -t %s" s)
    killSession s = spawn (printf "tmux kill-session -t %s" s)
    renameSession s = XP.mkXPrompt (namedPrompt "Enter new name:") myXPConfig (XP.mkComplFunFromList []) (spawn . printf "tmux rename-session -t %s %s" s)

runInTerminal :: XP.XPConfig -> X ()
runInTerminal c = do
  cmds <- io getCommands
  mkXPrompt (namedShellPrompt "Run in terminal: ") c (getShellCompl cmds $ searchPredicate c) (spawn . printf "%s -e %s" terminalEmulator)

myKeymap :: [(String, X ())]
myKeymap =
    [ ("M-<Left>", sendMessage Shrink)
    , ("M-<Right>", sendMessage Expand)
    , ("M-<Down>", sendMessage MirrorShrink)
    , ("M-<Up>", sendMessage MirrorExpand)
    , ("C-M1-l"    , spawn screenLocker)
    , ("<Print>"   , spawn "maim -u $(date +\"$HOME/image/Screenshots/%FT%T.png\")")
    , ("M1-<Print>", spawn "maim -u -i $(xdotool getactivewindow) $(date +\"$HOME/image/Screenshots/%FT%T.png\")")
    , ( "S-<Print>"
      , spawn "maim -u -s $(date +\"$HOME/image/Screenshots/%FT%T.png\")"
      )
    , ( "C-<Print>"
      , spawn "maim -u | tee /tmp/img.png | xclip -target image/png -sel clipboard"
      )
    , ( "C-M1-<Print>"
      , spawn "maim -u -i $(xdotool getactivewindow) | tee /tmp/img.png | xclip -target image/png -sel clipboard"
      )
    , ( "C-S-<Print>"
      , spawn "maim -u -s | tee /tmp/img.png | xclip -target image/png -sel clipboard"
      )
    , ( "M-f"
      , sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts >> toggleSmartSpacing
      )
    , ( "M-m"
      , sendMessage $ Toggle MIRROR
      )
    , ("M-S-t", myTreeSelect)
    , ("M-p", XP.shellPrompt myXPConfig)
    , ("M-S-p", runInTerminal myXPConfig)
    , ("M-S-x", XP.xmonadPrompt myXPConfig)
    , ("C-M1-h", withFocused hideWindow)
    , ("C-M1-p", popOldestHiddenWindow)
    , ("<XF86AudioRaiseVolume>" , spawn "amixer set Master 2%+")
    , ("<XF86AudioLowerVolume>" , spawn "amixer set Master 2%-")
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<XF86MonBrightnessUp>", spawn "backlight update +10; notify-send -h string:x-canonical-private-synchronous:anything -t 500 'Current brightness' \"$(backlight query)%\"")
    , ("<XF86MonBrightnessDown>", spawn "backlight update -10; notify-send -h string:x-canonical-private-synchronous:anything -t 500 'Current brightness' \"$(backlight query)%\"")
    , ("M--", incScreenWindowSpacing 2)
    , ("M-=", decScreenWindowSpacing 2)
    ]

myMouse = [((mod4Mask, button3), \w -> focus w >> Flex.mouseResizeWindow w)]

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
    , ppLayout  = const ""
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
        $   mkToggle (MIRROR ?? NBFULL ?? EOT)
        $   tall ||| spiral (6 / 7)
    where tall = ResizableTall 1 (3/100) (1/2) []

myConfig xmproc = docks $ ewmh $ def
    { modMask            = mod4Mask
    , terminal           = terminalEmulator
    , layoutHook         = myLayoutHook
    , logHook            = myXmobar xmproc
    , manageHook         = myManageHook <+> def
    , normalBorderColor  = colorToStr base2
    , focusedBorderColor = colorToStr base2
    } `additionalKeysP` myKeymap
      `additionalMouseBindings` myMouse

main :: IO ()
main = do
    installSignalHandlers
    spawn "(ps -e | grep pasystray) || pasystray -a"
    spawn "(ps -e | grep nm-applet) || nm-applet"
    spawn "(ps -e | grep blueman-tray) || blueman-tray"
    xmproc <- spawnPipe "xmobar $HOME/.xmonad/xmobarrc"
    xmonad $ myConfig xmproc

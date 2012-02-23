import XMonad
import XMonad.Util.EZConfig
import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Layout.Reflect
import XMonad.Layout.Grid
import XMonad.Hooks.SetWMName

import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.ComboP
import XMonad.Layout.LayoutCombinators hiding ((|||))
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run
import Control.Monad (liftM2, when)
import Data.Monoid
import Graphics.X11
import System.Exit
import System.IO
import XMonad.Actions.Submap

import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W
import qualified Data.Map as M

myLayout = tiled ||| Full ||| reflectHoriz tiled ||| Grid
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = ResizableTall nmaster delta ratio []
 
    -- The default number of windows in the master pane
    nmaster = 1
 
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
 
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

promptExit = do
     response <- runProcessWithInput "dmenu" ["-p", "Really quit?"] "no\nyes\n"
     when (response == "yes") (io (exitWith ExitSuccess))         

main = do
     dzen <- spawnPipe myStatusBar
     volume <- spawnPipe myVolumeBar
     memory <- spawnPipe myMemoryBar
     cpu <- spawnPipe myCpuBar
     time <- spawnPipe myDateBar
     xmonad $ defaultConfig
         { modMask = mod5Mask 
         , terminal = "gnome-terminal"
   --      , logHook = dynamicLogWithPP (myPrettyPrinter dbus)
	 , layoutHook = desktopLayoutModifiers (myLayout)
         , startupHook = setWMName "LG3D"
         , logHook   = dynamicLogWithPP $ myDzenPP dzen
         } `additionalKeys`
         [ ((mod1Mask,               xK_Tab        ), windows W.focusDown) -- %! Move focus to the next        window
         , ((mod1Mask .|. shiftMask, xK_Tab        ), windows W.focusUp  ) -- %! Move focus to the previous window
         , ((mod5Mask .|. shiftMask, xK_numbersign ), spawn ("emacsclient -c -a \"\""))
         , ((mod5Mask,               xK_a), sendMessage MirrorShrink)
         , ((mod5Mask,               xK_z), sendMessage MirrorExpand)
         , ((mod5Mask .|. shiftMask, xK_q), promptExit)
         ]

myFont = "Consolas-10:Medium" -- "-*-montecarlo-terminus-r-normal-*-11-*-*-*-c-*-*-*"
myIconDir = "/home/tristan/.dzen"
myDzenFGColor = "#555555"
myDzenBGColor = "#222222"
myNormalFGColor = "#ffffff"
myNormalBGColor = "#0f0f0f"
myFocusedFGColor = "#f0f0f0"
myFocusedBGColor = "#333333"
myUrgentFGColor = "#0099ff"
myUrgentBGColor = "#0077ff"
myIconFGColor = "#777777"
myIconBGColor = "#0f0f0f"
mySeperatorColor = "#555555"
myVolUp = "/home/tristan/bin/volup.sh"
myVolDown = "/home/tristan/bin/voldown.sh"

myStatusBar = "dzen2 -x '1920' -y '0' -h '18' -w '1300' -ta 'l' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myVolumeBar = "/home/tristan/bin/volume.sh | dzen2 -x '3220' -y '0' -h '18' -w '80' -ta 'l' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "' -e 'button3=exit;button4=exec:" ++ myVolUp ++ ";button5=exec:" ++ myVolDown ++ "'"
myMemoryBar = "/home/tristan/bin/memory.sh | dzen2 -x '3300' -y '0' -h '18' -w '125' -ta 'c' -l '3' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "' -e 'button1=togglecollapse'"
myCpuBar = "gcpubar -fg '#aecf96' -bg '#37383a' -l 'CPU: ' -h 7 | dzen2 -x 3425 -w 125 -ta 'l' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myDateBar = "/home/tristan/bin/status.sh | dzen2 -x '3425' -y '0' -h '18' -w '415' -ta 'r' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"


-- dynamicLog pretty printer for dzen:
myDzenPP h = defaultPP
    { ppCurrent = wrap ("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()^i(" ++ myIconDir ++ "/corner.xbm)^fg(" ++ myNormalFGColor ++ ")") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppVisible = wrap ("^fg(" ++ myNormalFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p()^i(" ++ myIconDir ++ "/corner.xbm)^fg(" ++ myNormalFGColor ++ ")") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppHidden = wrap ("^i(" ++ myIconDir ++ "/corner.xbm)") "^fg()^bg()^p()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId -- don't use ^fg() here!!
    --, ppHiddenNoWindows = wrap ("^fg(" ++ myDzenFGColor ++ ")^bg()^p()^i(" ++ myIconDir ++ "/corner.xbm)") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppHiddenNoWindows = \wsId -> if wsId `notElem` staticWs then "" else wrap ("^fg(" ++ myDzenFGColor ++ ")^bg()^p()^i(" ++ myIconDir ++ "/corner.xbm)") "^fg()^bg()^p()" . dropIx $ wsId
    , ppUrgent = wrap (("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p()^i(" ++ myIconDir ++ "/corner.xbm)^fg(" ++ myUrgentFGColor ++ ")")) "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppSep = " "
    , ppWsSep = " "
    , ppTitle = dzenColor ("" ++ myNormalFGColor ++ "") "" . wrap "< " " >"
    , ppLayout = dzenColor ("" ++ myNormalFGColor ++ "") "" .
        (\x -> case x of
        "Hinted Full" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-full.xbm)"
        "Hinted ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-tall-right.xbm)"
        "Hinted Mirror ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-mirror-bottom.xbm)"
        --"Hinted combining Tabbed Bottom Simplest and Full with DragPane  Vertical 0.1 0.8" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-gimp.xbm)"
        "Hinted combining Tabbed Bottom Simplest and Full with TwoPane using Not (Role \"gimp-toolbox\")" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-gimp.xbm)"
        _ -> x
        )
    , ppOutput = hPutStrLn h
    }
    where
    dropIx wsId = if (':' `elem` wsId) then drop 2 wsId else wsId
    staticWs = ["1:irc", "2:www", "3:music", "4:misc", "5:xbmc"]

-- ------
-- myPrettyPrinter :: Connection -> PP
-- myPrettyPrinter dbus = defaultPP {
--     ppOutput  = outputThroughDBus dbus
--   , ppTitle   = pangoColor "#003366" . shorten 50 . pangoSanitize
--   , ppCurrent = pangoColor "#006666" . wrap "[" "]" . pangoSanitize
--   , ppVisible = pangoColor "#663366" . wrap "(" ")" . pangoSanitize
--   , ppHidden  = wrap " " " "
--   , ppUrgent  = pangoColor "red"
--   }

-- getWellKnownName :: Connection -> IO ()
-- getWellKnownName dbus = tryGetName `catchDyn` (\ (DBus.Error _ _) ->
--                                                  getWellKnownName dbus)
--   where
--    tryGetName = do
--      namereq <- newMethodCall serviceDBus pathDBus interfaceDBus "RequestName"
--      addArgs namereq [String "org.xmonad.Log", Word32 5]
--      sendWithReplyAndBlock dbus namereq 0
--      return ()

-- outputThroughDBus :: Connection -> String -> IO ()
-- outputThroughDBus dbus str = do
--   let str' = "<span font=\"Terminus 9 Bold\">" ++ str ++ "</span>"
--   msg <- newSignal "/org/xmonad/Log" "org.xmonad.Log" "Update"
--   addArgs msg [String str']
--   send dbus msg 0 `catchDyn` (\ (DBus.Error _ _ ) -> return 0)
--   return ()

-- pangoColor :: String -> String -> String
-- pangoColor fg = wrap left right
--  where
--   left  = "<span foreground=\"" ++ fg ++ "\">"
--   right = "</span>"

-- pangoSanitize :: String -> String
-- pangoSanitize = foldr sanitize ""
--  where
--   sanitize '>'  acc = "&gt;" ++ acc
--   sanitize '<'  acc = "&lt;" ++ acc
--   sanitize '\"' acc = "&quot;" ++ acc
--   sanitize '&'  acc = "&amp;" ++ acc
--   sanitize x    acc = x:acc
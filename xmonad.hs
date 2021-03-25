-------------------------------------------------------------------------------
--
--  Arquivo de configuração do Xmonad 
--
--      By Gabriel Grechuk.
--
-------------------------------------------------------------------------------

module Main (main) where
 -- Base
import XMonad
import System.Directory
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

import System.Exit

import XMonad

import XMonad.Config.Desktop

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Monitor
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Circle
import XMonad.Layout.DecorationMadness
import XMonad.Layout.NoBorders (noBorders, smartBorders)

import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce


    -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

    -- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

    -- Layouts
import XMonad.Layout.Grid
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

    -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Unicode
import XMonad.Prompt.XMonad
import Control.Arrow (first)

   -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

-------------------------------------------------------------------------------
--  Definições gerais para a WM. 
--
myTerminal :: String
myTerminal = "alacritty"

myFont :: String
myFont = "xft:Hack Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

myEmojiFont :: String
myEmojiFont = "xft:JoyPixels:regular:size=9:antialias=true:hinting=true"

myNormalBorderColor :: String
myNormalBorderColor = "#c9b8f3"

myFocusedBorderColor :: String
myFocusedBorderColor = "#9e4af3"

myBorderWidth :: Dimension
myBorderWidth = 2

myModMask :: KeyMask
myModMask = mod1Mask

myWorkspaces = [" Terminal ", " Web ", " Vim ", " Images ", " Video ", "Books", " Research ", "Music", " Games "]



-------------------------------------------------------------------------------
--  Hook de inicialização. 
--
myStartupHook :: X ()
myStartupHook = do
          spawnOnce "picom --experimental-backends &"
          spawnOnce "nitrogen --restore "
<<<<<<< HEAD
=======
          
--------------------------------------------------------------------------------
main = do
  xmonad  $ ewmh desktopConfig
    {
      startupHook        = myStartupHook
    , workspaces         = myWorkspaces
    , modMask            = myModMask
    , manageHook         = myManageHook <+> manageHook desktopConfig
    , layoutHook         = smartBorders $ desktopLayoutModifiers $ myLayouts
    , logHook            = dynamicLogString def >>= xmonadPropLog
    , handleEventHook    = handleEventHook def <+> fullscreenEventHook 
    , terminal           = myTerminal
    , borderWidth       = myBorderWidth
    , normalBorderColor  = myNormalBorderColor 
    , focusedBorderColor = myFocusedBorderColor
    }


    `additionalKeys` 
    [((0, xK_Print), spawn "gnome-screenshot -i")]
>>>>>>> 477a0bdfd9d89109603296fc7de10830b55fcd55



-------------------------------------------------------------------------------
--  Layouts. A maioria eu não uso, mas gosto de manter.
--

myLayouts = noBorders tiled ||| noBorders (Mirror tiled) ||| noBorders Full ||| noBorders Circle ||| noBorders Grid
  where
     tiled   = Tall nmaster delta ratio 
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100


-------------------------------------------------------------------------------
--  composição das janelas.
--
myManageHook = composeAll
    [ className =? "Gimp"           --> doFloat
    , (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , className =? "Steam"          --> doFloat
    , className =? "steam"          --> doFullFloat 
    , (isFullscreen --> doFullFloat) 
    ]



--------------------------------------------------------------------------------
--  main
--

wGap = 5
sGap = 10

main = do
  xmonad  $ ewmh desktopConfig
    {
      startupHook        = myStartupHook
    , workspaces         = myWorkspaces
    , modMask            = myModMask
    , borderWidth        = myBorderWidth
    , manageHook         = myManageHook <+> manageHook desktopConfig
    , layoutHook         = desktopLayoutModifiers $ spacingRaw True (Border sGap sGap sGap sGap) True (Border wGap wGap wGap wGap) True $ myLayouts
    , logHook            = dynamicLogString def >>= xmonadPropLog
    , handleEventHook    = handleEventHook def <+> fullscreenEventHook 
    , terminal           = myTerminal
    , normalBorderColor  = myNormalBorderColor 
    , focusedBorderColor = myFocusedBorderColor
    }


    `additionalKeys` 
    [((0, xK_Print), spawn "gnome-screenshot -i")]


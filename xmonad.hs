-------------------------------------------------------------------------------
--
--  Arquivo de configuração do Xmonad 
--
--      By Gabriel Grechuk.
--
-------------------------------------------------------------------------------

module Main (main) where

import System.Exit

import XMonad

import XMonad.Config.Desktop

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Monitor
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.DecorationMadness
import XMonad.Layout.NoBorders (noBorders, smartBorders)

import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce

-------------------------------------------------------------------------------
--  Definições para o sistema.
--

myTerminal :: String
myTerminal = "alacritty"

myNormalBorderColor :: String
myNormalBorderColor = "#c9b8f3"

myFocusedBorderColor :: String
myFocusedBorderColor = "#9e4af3"

myBorderWidth :: Dimension
myBorderWidth = 2

myModMask :: KeyMask
myModMask = mod1Mask

myWorkspaces = ["1:Terminal", "2:Web", "3:Vim", "4:Images", "5:Video", "6:Books", "7:Research", "8:Music", "9:Games"]




myStartupHook :: X ()
myStartupHook = do
          spawnOnce "picom --experimental-backends &"
          spawnOnce "nitrogen --restore "
          
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



-------------------------------------------------------------------------------
myLayouts = noBorders tiled ||| noBorders (Mirror tiled) ||| noBorders Full ||| noBorders Circle ||| noBorders Grid
  where
     tiled   = Tall nmaster delta ratio 
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100



-------------------------------------------------------------------------------
myManageHook = composeAll
    [ className =? "Gimp"           --> doFloat
    , (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , className =? "Steam"          --> doFloat
    , className =? "steam"          --> doFullFloat 
    , (isFullscreen --> doFullFloat) 
    ]

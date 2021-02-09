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

--------------------------------------------------------------------------------
main = do
  xmonad  $ ewmh desktopConfig
    { modMask            = mod1Mask 
    , manageHook         = myManageHook <+> manageHook desktopConfig
    , layoutHook         = smartBorders $ desktopLayoutModifiers $ myLayouts
    , logHook            = dynamicLogString def >>= xmonadPropLog
    , handleEventHook    = handleEventHook def <+> fullscreenEventHook 
    , terminal           = "alacritty"
    , normalBorderColor  = "#dddddd" 
    , focusedBorderColor = "#ff0000"  
    }


    `additionalKeys` 
    [((0, xK_Print), spawn "gnome-screenshot -i")]



myLayouts = noBorders tiled ||| noBorders (Mirror tiled) ||| noBorders Full ||| noBorders Circle ||| noBorders Grid
  where
     tiled   = Tall nmaster delta ratio 
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100



myManageHook = composeAll
    [ className =? "Gimp"           --> doFloat
    , (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
    , className =? "Cairo-dock"     --> doIgnore
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore 
    , className =? "Steam"          --> doFloat
    , className =? "steam"          --> doFullFloat 
    , className =? "MPlayer"        --> doFloat
    , (isFullscreen --> doFullFloat) 
    ]


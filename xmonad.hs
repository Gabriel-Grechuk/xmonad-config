-------------------------------------------------------------------------------
--
--  Arquivo de configuração do Xmonad 
--
--      By Gabriel Grechuk.
--
-------------------------------------------------------------------------------

module Main (main) where


-- IMPORTS
import XMonad

import XMonad.Config.Desktop

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks

import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce

import XMonad.Layout.Grid
import XMonad.Layout.Circle
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing



-- PROPRIEDADES DAS JANELAS.
myNormalBorderColor :: String
myNormalBorderColor = "#c9b8f3"

myFocusedBorderColor :: String
myFocusedBorderColor = "#9e4af3"

myBorderWidth :: Dimension
myBorderWidth = 2

wGap :: Integer
wGap = 5

sGap :: Integer
sGap = 10



-- COMPRTAMENTO DAS JANELAS.
myLayouts = avoidStruts $ tiled ||| (Mirror tiled) ||| Full ||| Circle ||| Grid
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

myManageHook = composeAll
    [ className =? "Gimp"           --> doFloat
    , (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , className =? "Steam"          --> doFloat
    , className =? "steam"          --> doFullFloat
    , (isFullscreen --> doFullFloat)
    ]



-- PROPRIEDADES E COMPORTAMENTO DE AMBIENTE.
myWorkspaces :: [[Char]]
myWorkspaces = ["Terminal", "Web", "Vim", "Images", "Video", "Books", "Research", "Music", "Games"]

myFont :: String
myFont = "xft:Hack Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod1Mask

myTerminal :: String
myTerminal = "alacritty"

myStartupHook :: X ()
myStartupHook = do
          spawnOnce "picom --experimental-backends &"
          spawnOnce "nitrogen --restore "



-- MAIN
main :: IO ()
main = do
  xmonad  $ ewmh desktopConfig
    {
      startupHook        = myStartupHook
    , workspaces         = myWorkspaces
    , modMask            = myModMask
    , borderWidth        = myBorderWidth
    , manageHook         = myManageHook <+> manageHook desktopConfig
    , layoutHook         = desktopLayoutModifiers $ spacingRaw True (Border sGap sGap sGap sGap) True (Border wGap wGap wGap wGap) True $ smartBorders $ myLayouts
    , logHook            = dynamicLogString def >>= xmonadPropLog
    , handleEventHook    = handleEventHook def <+> fullscreenEventHook 
    , terminal           = myTerminal
    , normalBorderColor  = myNormalBorderColor 
    , focusedBorderColor = myFocusedBorderColor
    }


    `additionalKeys` 
    [((0, xK_Print), spawn "gnome-screenshot -i")]


--------------------------------------------------------------------------------
-- | Example.hs
--
-- Example configuration file for xmonad using the latest recommended
-- features (e.g., 'desktopConfig').
module Main (main) where

--------------------------------------------------------------------------------
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

--import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.NoBorders (noBorders, smartBorders)

--import XMonad.Layout.ResizableTile (ResizableTall(..))
--import XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
--import XMonad.Prompt
--import XMonad.Prompt.ConfirmPrompt
--import XMonad.Prompt.Shell
import XMonad.Util.EZConfig

--------------------------------------------------------------------------------
main = do
--  spawn "xmobar" -- Start a task bar such as xmobar.

  -- Start xmonad using the main desktop configuration with a few
  -- simple overrides:
  xmonad  $ ewmh desktopConfig
--  { modMask    = mod4Mask -- Use the "Win" key for the mod key
    { modMask    = mod1Mask -- Use the "Alt" key for the mod key
    , manageHook = myManageHook <+> manageHook desktopConfig
    , layoutHook = smartBorders $ desktopLayoutModifiers $ myLayouts
    , logHook    = dynamicLogString def >>= xmonadPropLog
    , handleEventHook = handleEventHook def <+> fullscreenEventHook 
    , terminal   =  "alacritty"
    , normalBorderColor  = "gray" -- "#dddddd"
    , focusedBorderColor = "rebeccapurple"  -- "#ff0000" don't use hex, not <24 bit safe
    }

    `additionalKeys` -- Add some extra key bindings:
    [     ((0, xK_Print), spawn "spectacle")
--      , ("M-S-q",   confirmPrompt myXPConfig "exit" (io exitSuccess))
--      , ("M-p",     shellPrompt myXPConfig)
--      , ("M-<Esc>", sendMessage (Toggle "Full"))
    ]

--------------------------------------------------------------------------------
-- | Customize layouts.
--
-- This layout configuration uses two primary layouts, 'ResizableTall'
-- and 'BinarySpacePartition'.  You can also use the 'M-<Esc>' key
-- binding defined above to toggle between the current layout and a
-- full screen layout.
--
-- myLayouts = toggleLayouts (noBorders Full) others
--   where
--     others = ResizableTall 1 (1.5/100) (3/5) [] ||| emptyBSP

myLayouts = noBorders tiled ||| noBorders (Mirror tiled) ||| noBorders Full ||| noBorders Circle ||| noBorders Grid
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     
     -- The default number of windows in the master pane
     nmaster = 1
     
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
     
--------------------------------------------------------------------------------
-- | Customize the way 'XMonad.Prompt' looks and behaves.  It's a
-- great replacement for dzen.
-- myXPConfig = def
--  { position          = Top
--  , alwaysHighlight   = True
--  , promptBorderWidth = 0
--  , font              = "xft:monospace:size=9"
--  }

--------------------------------------------------------------------------------
-- | Manipulate windows as they are created.  The list given to
-- @composeOne@ is processed from top to bottom.  The first matching
-- rule wins.
--
-- Use the `xprop' tool to get the info you need for these matches.
-- For className, use the second value that xprop gives you.
myManageHook = composeAll
    [ className =? "Gimp"           --> doFloat
    , (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
    , className =? "Cairo-dock"     --> doIgnore
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore 
    , className =? "Steam"          --> doFloat
    , className =? "steam"          --> doFullFloat -- bigpicture-mode
    , className =? "MPlayer"        --> doFloat
    , (isFullscreen --> doFullFloat) 
    ]

--myManageHook = composeOne
--  [ className =? "Pidgin" -?> doFloat
--  , className =? "XCalc"  -?> doFloat
--  , className =? "mpv"    -?> doFloat
--  , isDialog              -?> doCenterFloat

    -- Move transient windows to their parent:
--  , transience
--  ]

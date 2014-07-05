import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

import XMonad.Actions.WindowGo

import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Layout.Named

-- for keyboard shortcut mod4 is windows ShortcutKey
modm = mod4Mask

tall = Tall 1 (3/100) (1/2)

main = do
        xmproc <- spawnPipe "/usr/bin/xmobar /home/tsu-nera/.xmobarrc"
        xmonad $ defaultConfig
          { manageHook = manageDocks <+> manageHook defaultConfig
          --, layoutHook = avoidStruts  $  layoutHook defaultConfig
	  , layoutHook = mkToggle1 FULL $ desktopLayoutModifiers (named "V" tall ||| (named "H" $ Mirror tall))

	  -- sometimes, xmonad freese,comment out these lines 
          -- , logHook = dynamicLogWithPP $ xmobarPP
          -- { ppOutput = hPutStrLn xmproc
          --    , ppTitle = xmobarColor "green" "" . shorten 50
          -- }

          -- Border settings
 	  , borderWidth = 2
          , normalBorderColor  = "#99ccff"
          , focusedBorderColor = "#0033dd" -- blue

          -- Rebind Mod to the Hiragana_Katakana 
	  , modMask = mod3Mask
	  -- , modMask = mod1Mask   	 

     	  -- use rxvt-unicode 
	  , terminal = "urxvt" 
          } 
          `additionalKeys`
          [
          ((modm, xK_e), runOrRaise "emacs" (className =? "Emacs"))
          , ((modm, xK_t), runOrRaise "urxvt" (className =? "URxvt"))
          , ((modm, xK_g), runOrRaise "chrome" (className =? "Google-chrome"))
	  , ((modm, xK_f), sendMessage (Toggle FULL))
          ]

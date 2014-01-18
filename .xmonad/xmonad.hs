import XMonad

main = do
     xmonad $ defaultConfig {
    modMask  = mod4Mask,
    terminal = "urxvt"
  } 




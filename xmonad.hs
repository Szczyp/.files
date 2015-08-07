import XMonad
import XMonad.ManageHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig

tiled = Tall nmaster delta ratio
  where nmaster = 1
        ratio = 1/2
        delta = 1/8

keybindings = [ ("M-p", spawn "dmenu_run -i -fn 'Fira Sans Medium-13'")
              , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -5%")
              , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +5%")
              , ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle")
              ]

ignore = composeAll [ title =? "LIMBO" --> doIgnore ]

main = do
  xmonad $ ewmh defaultConfig
    { borderWidth     = 0
    , layoutHook      = tiled ||| Full
    , manageHook      = ignore
    , handleEventHook = fullscreenEventHook
    , startupHook     = spawn "compton -bf --inactive-dim 0.05 --backend glx"
    , modMask         = mod4Mask
    } `additionalKeysP` keybindings

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig (additionalKeys)

layout = tiled ||| Full
  where tiled = Tall nmaster delta ratio
        nmaster = 1
        ratio = 1/2
        delta = 1/8

main :: IO ()
main = do
  spawn "xrdb -merge ~/.Xresources"
  spawn "compton -bf -i 0.9 -e 0.9 --sw-opti"
  xmonad $ defaultConfig
    { manageHook      = manageDocks <+> fullscreenManageHook <+> manageHook defaultConfig
    , handleEventHook = fullscreenEventHook
    , layoutHook      = noBorders . fullscreenFull . avoidStruts $ layout
    , modMask         = mod4Mask
    } `additionalKeys` [ ((mod4Mask, xK_p), spawn "dmenu_run -i") ]

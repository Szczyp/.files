import Data.Monoid
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig (additionalKeys)


main :: IO ()
main = do
  spawn "xrdb -merge ~/.Xresources"
  spawn "compton -bf -i 0.9 -e 0.9 --sw-opti"
  xmonad $ defaultConfig
    { manageHook      = manageDocks <+> fullscreenManageHook <+> manageHook defaultConfig
    , handleEventHook = fullscreenEventHook
    , layoutHook      = noBorders . fullscreenFull . avoidStruts $ layoutHook defaultConfig
    , modMask         = mod4Mask
    } `additionalKeys` [ ((mod4Mask, xK_p), spawn "dmenu_run -i") ]
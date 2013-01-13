import Data.Monoid
import XMonad
import XMonad.Config.Gnome
-- import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig (additionalKeys)


main :: IO ()
main = do
  spawn "xcompmgr -n"
  xmonad $ gnomeConfig
    { startupHook     = setWMName "LG3D"
    , manageHook      = manageDocks <+> fullscreenManageHook <+> manageHook gnomeConfig
    , handleEventHook = fullscreenEventHook
    , layoutHook      = smartBorders . fullscreenFull . avoidStruts $ layoutHook gnomeConfig
    , modMask         = mod4Mask
    -- , logHook         = takeTopFocus
    } `additionalKeys` [ ((mod4Mask, xK_d), spawn "dmenu_run -b -i") ]
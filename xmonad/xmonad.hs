import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.EwmhDesktops
import System.IO
import Data.Map.Lazy as M
import XMonad.Actions.Submap (submap)
import qualified XMonad.Util.ExtensibleState as XS
import qualified Control.Concurrent.STM as STM
import qualified System.Environment as Env
import XMonad.Layout.Tabbed
import XMonad.Config.Xfce

commandSubMap = submap (M.fromList [
    ((0, xK_l), spawn "xscreensaver-command -lock")
  ])

main = do
    xmonad (xfceConfig
        { borderWidth = 5 
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , terminal = "st -f 'Roboto Mono:size=10'"
        }
        `additionalKeys`
        [ ((mod4Mask, xK_s), commandSubMap),
          ((mod4Mask, xK_p), spawn "rofi -show run"),
          ((mod4Mask, xK_o), spawn "rofi -show ssh")          
          ]
          )

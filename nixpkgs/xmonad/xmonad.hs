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
import XMonad.Layout ((|||))
import qualified XMonad.Layout.Tabbed as Tabbed

commandSubMap xmobarPipeBox = submap (M.fromList [
    ((0, xK_l), spawn "xscreensaver-command -lock")
  ])

data KeyboardLayouts = KeyboardUS | KeyboardNeo deriving (Read, Show, Typeable)

instance ExtensionClass KeyboardLayouts where
        initialValue = KeyboardNeo
        extensionType = PersistentExtension

toggleKeyboardLayout = do
        actual <- XS.get
        case actual of
                KeyboardNeo -> do
                        spawn "setxkbmap us"
                        XS.put KeyboardUS
                        refresh
                KeyboardUS -> do
                        spawn "setxkbmap de neo"
                        XS.put KeyboardNeo
                        refresh

getKeyboardLayout = do
        actual <- XS.get :: X KeyboardLayouts
        case actual of
                KeyboardNeo -> return (Just (xmobarColor "red" "black" " NEO  "))
                KeyboardUS -> return (Just (xmobarColor "green" "black" "  US  "))

main = do
    xmproc <- spawnPipe "xmobar ~/.xmobarrc"
    xmobarPipeBox <- STM.atomically $ STM.newTVar xmproc
    xmonad $ docks def
        { borderWidth = 5 
        , manageHook = manageDocks <+> manageHook def
        , layoutHook = avoidStruts  $  Tabbed.simpleTabbed ||| layoutHook def
        , handleEventHook = fullscreenEventHook
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = \line -> do
                                xmproc <- STM.atomically $ STM.readTVar xmobarPipeBox
                                hPutStrLn xmproc line
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        , ppExtras = ppExtras xmobarPP ++ [ getKeyboardLayout ]
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , terminal = "st -f 'Roboto Mono:size=12'"
        }
        `additionalKeys`
        [ ((mod4Mask, xK_s), commandSubMap xmobarPipeBox),
          ((mod4Mask, xK_BackSpace), toggleKeyboardLayout),
          ((mod4Mask, xK_p), spawn "rofi -show run"),
          ((mod4Mask, xK_o), spawn "rofi -show ssh")          
          ]

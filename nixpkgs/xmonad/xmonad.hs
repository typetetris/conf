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

commandSubMap xmobarPipeBox = submap (M.fromList [
    ((0, xK_l), spawn "xscreensaver-command -lock"),
    ((0, xK_d), toggleDPI xmobarPipeBox)
  ])

data KeyboardLayouts = KeyboardUS | KeyboardNeo deriving (Read, Show, Typeable)

instance ExtensionClass KeyboardLayouts where
        initialValue = KeyboardNeo
        extensionType = PersistentExtension

data DisplayDPI = DPI192 | DPI110 deriving (Read, Show, Typeable)

instance ExtensionClass DisplayDPI where
        initialValue = DPI192
        extensionType = PersistentExtension

toggleDpiValue DPI192 = DPI110
toggleDpiValue DPI110 = DPI192

dpiValue DPI192 = "192"
dpiValue DPI110 = "110"

scaleValues DPI192 = ("2", "0.5")
scaleValues DPI110 = ("1", "1")

setupDisplays DPI192 = spawn "xrandr --output eDP1 --auto --output DP1-1 --off            --output DP2-2 --off"
setupDisplays DPI110 = spawn "xrandr --output eDP1 --off  --output DP2-2 --primary --auto --output DP1-1 --right-of DP2-2 --auto"

setDPI :: (MonadIO m) => STM.TVar Handle -> String -> (String, String) -> m ()
setDPI xmobarPipeBox value (gdkScale, gdkDpiScale) = do
        spawn ("xrandr --dpi " ++ value)
--        liftIO $ Env.setEnv "GDK_SCALE" gdkScale
--        liftIO $ Env.setEnv "GDK_DPI_SCALE" gdkDpiScale
        liftIO $ Env.unsetEnv "GDK_SCALE"
        liftIO $ Env.unsetEnv "GDK_DPI_SCALE"
        spawn ("xrdb -merge <<<\"Xft.dpi: " ++ value ++"\"")
        newXmobar <- spawnPipe "xmobar ~/.xmobarrc"
        oldPipe <- liftIO $ STM.atomically $ do
                oldPipe <- STM.readTVar xmobarPipeBox
                STM.writeTVar xmobarPipeBox newXmobar
                return oldPipe
        liftIO $ hClose oldPipe

toggleDPI xmobarPipeBox = do
        actual <- XS.get
        let next = toggleDpiValue actual
        setupDisplays next
        setDPI xmobarPipeBox (dpiValue next) (scaleValues next)
        XS.put next
        refresh

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
getDpiValue = do
        actual <- XS.get :: X DisplayDPI
        return (Just (xmobarColor "yellow" "black" ("DPI: " ++ dpiValue actual)))

main = do
    xmproc <- spawnPipe "xmobar ~/.xmobarrc"
    xmobarPipeBox <- STM.atomically $ STM.newTVar xmproc
    let actual = initialValue
    setupDisplays actual
    setDPI xmobarPipeBox (dpiValue actual) (scaleValues actual)
    xmonad $ docks def
        { borderWidth = 5 
        , manageHook = manageDocks <+> manageHook def
        , layoutHook = avoidStruts  $  layoutHook def
        , handleEventHook = fullscreenEventHook
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = \line -> do
                                xmproc <- STM.atomically $ STM.readTVar xmobarPipeBox
                                hPutStrLn xmproc line
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        , ppExtras = ppExtras xmobarPP ++ [ getKeyboardLayout, getDpiValue ]
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , terminal = "st -f 'Roboto Mono:size=14'"
        }
        `additionalKeys`
        [ ((mod4Mask, xK_s), commandSubMap xmobarPipeBox),
          ((mod4Mask, xK_BackSpace), toggleKeyboardLayout),
          ((mod4Mask, xK_p), spawn "rofi -show run"),
          ((mod4Mask, xK_o), spawn "rofi -show ssh")          
          ]

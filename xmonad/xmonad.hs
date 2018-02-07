import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Circle
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Fullscreen
import XMonad.Util.EZConfig (additionalKeys)

myModMask= mod4Mask

focusedColor :: String
focusedColor = "#e80cb5"

greenColor :: String
greenColor = "#98c379"

bgColor :: String
bgColor = "#38032c"

data MyWorkspace = WebWorkspace
                 | MailWorkspace
                 | CodeWorkspace
                 | ChatWorkspace
                 | WinWorkspace
                 | MusicWorkspace
                 deriving (Enum, Bounded)

myWorkspaces :: [MyWorkspace]
myWorkspaces = enumFrom minBound

toWorkspaceId :: MyWorkspace -> WorkspaceId
toWorkspaceId WebWorkspace = "web"
toWorkspaceId MailWorkspace = "mail"
toWorkspaceId CodeWorkspace = "code"
toWorkspaceId ChatWorkspace = "chat"
toWorkspaceId WinWorkspace = "win"
toWorkspaceId MusicWorkspace = "music"

myLayout = onWorkspace (toWorkspaceId ChatWorkspace) Circle .
           onWorkspace (toWorkspaceId CodeWorkspace) (ResizableTall 1 (3/100) (4/7) []) .
           onWorkspace (toWorkspaceId MusicWorkspace) Circle .
           layoutHook $ def

myManageHook = composeAll [ appName =? "irssi" --> doShift "chat"
                          , appName =? "vimpc" --> doFloat
                          , appName =? "vimpc" --> doShift "music"]

main :: IO ()
main = xmonad . fullscreenSupport $ desktopConfig { layoutHook = avoidStruts . smartSpacingWithEdge 5 $ myLayout
                                                         , workspaces = map toWorkspaceId $ myWorkspaces
                                                         , terminal = "kitty"
                                                         , borderWidth = 2
                                                         , focusedBorderColor = focusedColor
                                                         , normalBorderColor = bgColor
                                                         , manageHook = myManageHook <+> manageHook desktopConfig
                                                         , modMask = myModMask
                                                         , startupHook = docksStartupHook <+> spawn "/usr/bin/taffybar" <+> startupHook desktopConfig
							 , handleEventHook = docksEventHook <+> handleEventHook desktopConfig
                                                         } `additionalKeys` [ ((myModMask, xK_b), sendMessage ToggleStruts)
					                                    , ((myModMask, xK_space), spawn "rofi -show run")
                                                                            ]
                                                

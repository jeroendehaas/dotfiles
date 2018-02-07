import System.Taffybar
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Pager
import System.Taffybar.SimpleClock
import System.Taffybar.NetMonitor
import System.Taffybar.Widgets.PollingGraph
import System.Information.CPU
import System.Information.Memory
import System.Taffybar.Widgets.PollingLabel

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [ totalLoad, systemLoad ]

memoryPercentage :: IO Double
memoryPercentage = memoryUsedRatio <$> parseMeminfo

memoryCallback :: IO [Double]
memoryCallback = return <$> memoryPercentage

simplifyLayout :: String -> String
simplifyLayout l = case words l of
  [] -> ""
  xs -> last xs

myPagerConfig :: PagerConfig
myPagerConfig = PagerConfig
  { activeWindow     = escape . shorten 40
  , activeLayout     = escape . simplifyLayout
  , activeWorkspace  = colorize "green" "" . wrap "[" "]" . escape
  , hiddenWorkspace  = escape
  , emptyWorkspace   = colorize "#b3b3b3" "" . escape
  , visibleWorkspace = wrap "(" ")" . escape
  , urgentWorkspace  = colorize "red" "yellow" . escape
  , widgetSep        = " : "
  }

main = do
  let cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1), (1, 0, 1, 0.5)]
                                  , graphLabel = Just "cpu"
                                  , graphBackgroundColor = (0.9, 0.9, 0.9)
                                  }
      memCfg = defaultGraphConfig { graphDataColors = [ (1, 0.7, 0, 1) ]
                                  , graphLabel = Just "mem"
                                  , graphBackgroundColor = (0.9, 0.9, 0.9)
                                  }
      clock = textClockNew Nothing "<span fgcolor='#4ec2e8'>%a %b %_d %H:%M</span>" 1
      pager = taffyPagerNew myPagerConfig
      tray = systrayNew
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      mem = pollingGraphNew memCfg 0.5 memoryCallback
      memTxt = pollingLabelNew "" 0.5 $ (\d -> show d ++ "%") <$> memoryPercentage
      net = netMonitorNew 0.5 "br0"
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ pager ]
                                        , endWidgets = [ tray, clock, net, memTxt, mem, cpu ]
                                        }

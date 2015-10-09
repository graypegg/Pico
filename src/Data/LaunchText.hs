module Data.LaunchText where
import Paths_Pico (version)
import Data.Version (showVersion)

launchText :: String
launchText = "Pico version " ++ (showVersion version) ++ "\nUsing config file `~/.pico_config`\nInteractive Mode, Run program with HALT command"
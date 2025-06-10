module MakeDeps (frontendPlugin) where

import GHC.Plugins
import MakeDeps.MakeFile (doMkDependHS)

frontendPlugin :: FrontendPlugin
frontendPlugin =
    defaultFrontendPlugin
        { frontend = doMkDependHS
        }

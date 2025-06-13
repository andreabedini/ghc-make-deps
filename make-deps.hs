import GHC (
    defaultErrorHandler,
    getLogger,
    getSessionDynFlags,
    noLoc,
    parseDynamicFlags,
    runGhc,
    setSessionDynFlags,
    unLoc,
 )
import GHC.Driver.Session (defaultFatalMessager, defaultFlushOut)
import GHC.SysTools.BaseDir (findTopDir)
import MakeFile (doMkDependHS)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    topdir <- findTopDir Nothing
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
        runGhc (Just topdir) $ do
            dflags <- getSessionDynFlags
            logger <- getLogger
            (dflags1, fileish_args, _dynamicFlagWarnings) <- parseDynamicFlags logger dflags (map noLoc args)
            setSessionDynFlags dflags1
            doMkDependHS (map unLoc fileish_args)

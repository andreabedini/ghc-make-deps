{-# LANGUAGE LambdaCase #-}
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_)
import GHC
import GHC.Data.Graph.Directed (SCC (..))
import GHC.Data.Maybe
import GHC.Data.OsPath (unsafeDecodeUtf)
import GHC.Driver.Errors.Types (DriverMessage (..), GhcMessage (..))
import GHC.Driver.Make (cyclicModuleErr)
import GHC.Driver.Session (defaultFatalMessager, defaultFlushOut)
import GHC.Iface.Errors (cannotFindModule)
import GHC.Iface.Errors.Types (IfaceMessage (..), InterfaceLookingFor (..))
import GHC.SysTools.BaseDir (findTopDir)
import GHC.Types.SourceError (throwOneError)
import GHC.Unit.Finder
import GHC.Unit.Module.Graph (ModuleGraphNode (..))
import GHC.Unit.Module.Location
import GHC.Unit.Module.ModSummary
import GHC.Utils.Error (debugTraceMsg, mkPlainErrorMsgEnvelope)
import GHC.Utils.Outputable
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath

import Data.Set qualified as Set
import Data.Traversable (for)

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

doMkDependHS :: GhcMonad m => [FilePath] -> m ()
doMkDependHS srcs = do
    logger <- getLogger

    -- Initialisation
    dflags0 <- GHC.getSessionDynFlags

    -- We kludge things a bit for dependency generation. Rather than
    -- generating dependencies for each way separately, we generate
    -- them once and then duplicate them for each way's osuf/hisuf.
    -- We therefore do the initial dependency generation with an empty
    -- way and .o/.hi extensions, regardless of any flags that might
    -- be specified.
    let dflags1 =
            dflags0
                { targetWays_ = Set.empty
                , hiSuf_ = "hi"
                , objectSuf_ = "o"
                }
    GHC.setSessionDynFlags dflags1

    -- If no suffix is provided, use the default -- the empty one
    let dflags =
            if null (depSuffixes dflags1)
                then dflags1{depSuffixes = [""]}
                else dflags1

    -- Do the downsweep to find all the modules
    targets <- for srcs (\s -> GHC.guessTarget s Nothing Nothing) 
    GHC.setTargets targets
    let excl_mods = depExcludeMods dflags
    module_graph <- GHC.depanal excl_mods True {- Allow dup roots -}

    -- Sort into dependency order
    -- There should be no cycles
    let sorted = GHC.topSortModuleGraph False module_graph Nothing

    -- Print out the dependencies if wanted
    liftIO $ debugTraceMsg logger 2 (text "Module dependencies" $$ ppr sorted)

    -- Process them one by one, dumping results into makefile
    -- and complaining about cycles
    hsc_env <- getSession
    root <- liftIO getCurrentDirectory
    for_ sorted $ \case
        (CyclicSCC nodes) ->
            -- There shouldn't be any cycles; report them
            throwOneError $ cyclicModuleErr nodes
        -- Instantiation nodes track the instantiation of other units (backpack dependencies) with the holes (signatures) of the current package.
        (AcyclicSCC (InstantiationNode _uid node)) ->
            -- There shouldn't be any backpack instantiations; report them as well
            throwOneError $
                mkPlainErrorMsgEnvelope noSrcSpan $
                    GhcDriverMessage $
                        DriverInstantiationNodeInDependencyGeneration node
        -- Link nodes are whether are are creating a linked product (ie executable/shared object etc) for a unit.
        (AcyclicSCC (LinkNode{})) ->
            -- TODO: Nothing here?
            return ()
        -- There is a module summary node for each module, signature, and boot module being built.
        (AcyclicSCC (ModuleNode _ node)) ->
            processDeps dflags hsc_env excl_mods root node

processDeps :: (GhcMonad m) => DynFlags -> HscEnv -> [ModuleName] -> FilePath -> ModSummary -> m ()
processDeps dflags hsc_env excl_mods root node = do
    -- Emit std dependency of the object(s) on the source file
    -- Something like       A.o : A.hs
    liftIO $ writeDependency root obj_files src_file

    -- add dependency between objects and their corresponding .hi-boot
    -- files if the module has a corresponding .hs-boot file (#14482)
    when (isBootSummary node == IsBoot) $ do
        let hi_boot = msHiFilePath node
        let obj = unsafeDecodeUtf $ removeBootSuffix (msObjFileOsPath node)
        for_ extra_suffixes $ \suff -> do
            let way_obj = insertSuffixes obj [suff]
            let way_hi_boot = insertSuffixes hi_boot [suff]
            for_ way_hi_boot $ liftIO . writeDependency root way_obj

    -- Emit a dependency for each CPP import
    when (depIncludeCppDeps dflags) $ do
        -- CPP deps are discovered in the module parsing phase by parsing
        -- comment lines left by the preprocessor.
        -- Note that GHC.parseModule may throw an exception if the module
        -- fails to parse, which may not be desirable (see #16616).
        parsedMod <- GHC.parseModule node
        for_ (GHC.pm_extra_src_files parsedMod) $ liftIO . writeDependency root obj_files

    -- Source imports of the module
    for_ (ms_srcimps node) $ \(mb_pkg, L loc mn) -> do
        unless (mn `elem` excl_mods) $
            do_imp loc IsBoot mb_pkg mn

    -- Textual imports, plus plugin imports but not SOURCE imports
    for_ (ms_imps node) $ \(mb_pkg, L loc mn) -> do
        unless (mn `elem` excl_mods) $
            do_imp loc NotBoot mb_pkg mn
  where
    extra_suffixes = depSuffixes dflags
    include_pkg_deps = depIncludePkgDeps dflags
    src_file = msHsFilePath node
    
    obj_file = msObjFilePath node
    obj_files = insertSuffixes obj_file extra_suffixes

    do_imp ::
        (GhcMonad m, MonadIO m) =>
        SrcSpan -> IsBootInterface -> PkgQual -> ModuleName -> m ()
    do_imp loc is_boot pkg_qual imp_mod = do
        mb_hi <- liftIO $ findDependency hsc_env loc pkg_qual imp_mod is_boot include_pkg_deps
        for_ mb_hi $ \hi_file -> do
            let hi_files = insertSuffixes hi_file extra_suffixes
            for_ (obj_files `zip` hi_files) $ \(obj, hi) ->
                liftIO $ writeDependency root [obj] hi

writeDependency :: FilePath -> [String] -> FilePath -> IO ()
writeDependency root targets dep =
   putStrLn $ unwords targets ++ " : " ++ makeRelative root dep

findDependency ::
    HscEnv ->
    SrcSpan ->
    PkgQual -> -- package qualifier, if any
    ModuleName -> -- Imported module
    IsBootInterface -> -- Source import
    Bool -> -- Record dependency on package modules
    IO (Maybe FilePath) -- Interface file
findDependency hsc_env srcloc pkg imp is_boot include_pkg_deps = do
    -- Find the module; this will be fast because
    -- we've done it once during downsweep
    findImportedModule hsc_env imp pkg >>= \case
        Found loc _module
            -- Home package: just depend on the .hi or hi-boot file
            | isJust (ml_hs_file loc) || include_pkg_deps ->
                return (Just (unsafeDecodeUtf $ addBootSuffix_maybe is_boot (ml_hi_file_ospath loc)))
            -- Not in this package: we don't need a dependency
            | otherwise ->
                return Nothing
        failure ->
            throwOneError $
                mkPlainErrorMsgEnvelope srcloc $
                    GhcDriverMessage $
                        DriverInterfaceError $
                            Can'tFindInterface (cannotFindModule hsc_env imp failure) (LookingForModule imp is_boot)

-----------------------------
insertSuffixes ::
    FilePath -> -- Original filename;   e.g. "foo.o"
    [String] -> -- Suffix prefixes      e.g. ["x_", "y_"]
    [FilePath] -- Zapped filenames     e.g. ["foo.x_o", "foo.y_o"]
    -- Note that the extra bit gets inserted *before* the old suffix
    -- We assume the old suffix contains no dots, so we know where to
    -- split it
insertSuffixes file_name extras =
    [basename <.> (extra ++ suffix) | extra <- extras]
  where
    (basename, suffix) = case splitExtension file_name of
        -- Drop the "." from the extension
        (b, s) -> (b, drop 1 s)

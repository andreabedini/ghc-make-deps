{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module MakeDeps.MakeFile.JSON (
    writeJSONFile,
    JsonOutput (..),
    mkJsonOutput,
    updateJson,
    writeJsonOutput,
    DepJSON,
    DepNode (..),
    Dep (..),
    initDepJSON,
    updateDepJSON,
)
where

import Data.Foldable (traverse_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import GHC.Data.FastString (FastString, lexicalCompareFS, unpackFS)
#if __GLASGOW_HASKELL__ >= 912
import GHC.Data.OsPath (unsafeDecodeUtf)
#else
import GHC.Utils.Panic (panic)
#endif
import GHC.Generics (Generic, Generically (Generically))
import GHC.Unit (
    GenModule (..),
    GenericUnitInfo,
    IsBootInterface (..),
    Module,
    ModuleName,
    PackageName (..),
    UnitId,
    UnitInfo,
    moduleNameString,
    unitIdString,
 )
import GHC.Utils.Json (
    JsonDoc (..),
    ToJson (..),
    renderJSON,
 )
import GHC.Utils.Misc (
  withAtomicRename,
#if __GLASGOW_HASKELL__ < 912
  HasCallStack,
#endif
 )
import GHC.Utils.Outputable as O (showSDocUnsafe)
import System.FilePath ()
import System.OsPath

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC.Unit.Info qualified as GHC

#if __GLASGOW_HASKELL__ < 912
unsafeDecodeUtf :: HasCallStack => OsPath -> FilePath
unsafeDecodeUtf p = either (\err -> panic $ "Failed to decodeUtf \"" ++ show p ++ "\", because: " ++ show err) id (decodeUtf p)
#endif

newtype PackageId = PackageIdX GHC.PackageId
    deriving newtype Eq

{-# COMPLETE PackageId #-}
pattern PackageId :: FastString -> PackageId
pattern PackageId pkg = PackageIdX (GHC.PackageId pkg)

unitPackageId :: GenericUnitInfo GHC.PackageId srcpkgname uid modulename mod -> PackageId
unitPackageId = PackageIdX . GHC.unitPackageId

instance Ord PackageId where
    compare :: PackageId -> PackageId -> Ordering
    compare (PackageId a) (PackageId b) = lexicalCompareFS a b

-- unitPackageName :: PackageId -> PackageName
-- unitPackageName (PackageId pkgId) = GHC.unitPackageName pkgId

--------------------------------------------------------------------------------
-- Output helpers
--------------------------------------------------------------------------------

writeJSONFile :: ToJson a => a -> FilePath -> IO ()
writeJSONFile doc p = do
    withAtomicRename p $
        \tmp -> writeFile tmp $ O.showSDocUnsafe $ renderJSON $ json doc

--------------------------------------------------------------------------------
-- Output interface for json dumps
--------------------------------------------------------------------------------

-- | Resources for a json dump option, used in "GHC.Driver.MakeFile".
-- The flag @-dep-json@ add an additional output target for dependency
-- diagnostics.
data JsonOutput a
    = JsonOutput
    { json_ref :: IORef a
    -- ^ This ref is updated in @processDeps@ incrementally, using a
    -- flag-specific type.
    , json_path :: FilePath
    -- ^ The output file path specified as argument to the flag.
    }

-- | Allocate an 'IORef' with the given function if the 'FilePath' is 'Just',
-- indicating that the userspecified @-*-json@.
mkJsonOutput ::
    IO (IORef a) ->
    IO (JsonOutput a)
mkJsonOutput mk_ref = do
    json_ref <- mk_ref
    pure JsonOutput{json_ref, json_path = "deps.json"}

-- | Update the dump data in 'json_ref' if the output target is present.
updateJson :: Maybe (JsonOutput a) -> (a -> a) -> IO ()
updateJson out f = traverse_ (\JsonOutput{json_ref} -> modifyIORef' json_ref f) out

-- | Write a json object to the flag-dependent file if the output target is
-- present.
writeJsonOutput ::
    ToJson a =>
    Maybe (JsonOutput a) ->
    IO ()
writeJsonOutput =
    traverse_ $ \JsonOutput{json_ref, json_path} -> do
        payload <- readIORef json_ref
        writeJSONFile payload json_path

--------------------------------------------------------------------------------
-- Types abstracting over json and Makefile
--------------------------------------------------------------------------------

data DepNode
    = DepNode
    { dn_mod :: Module
    , dn_src :: OsPath
    , dn_obj :: OsPath
    , dn_hi :: OsPath
    , dn_boot :: IsBootInterface
    , dn_options :: Set.Set String
    }

data Dep
    = DepHi
        { dep_mod :: Module
        , dep_path :: OsPath
        , dep_unit :: Maybe UnitInfo
        , dep_local :: Bool
        , dep_boot :: IsBootInterface
        }
    | DepCpp
        { dep_path_cpp :: OsPath
        }

--------------------------------------------------------------------------------
-- Payload for -dep-json
--------------------------------------------------------------------------------

newtype PackageDeps
    = PackageDeps (Map.Map (String, UnitId, PackageId) (Set.Set ModuleName))
    deriving newtype Monoid

instance Semigroup PackageDeps where
    PackageDeps l <> PackageDeps r = PackageDeps (Map.unionWith (<>) l r)

data Deps
    = Deps
    { sources :: Set.Set OsPath
    , modules :: (Set.Set ModuleName, Set.Set ModuleName)
    , packages :: PackageDeps
    , cpp :: Set.Set OsPath
    , options :: Set.Set String
    , preprocessor :: Maybe FilePath
    }
    deriving stock Generic
    deriving (Monoid, Semigroup) via (Generically Deps)

data ModuleDeps
    = ModuleDeps
    { source :: Deps
    , boot :: Maybe Deps
    }
    deriving stock Generic
    deriving (Monoid, Semigroup) via (Generically ModuleDeps)

newtype DepJSON = DepJSON (Map.Map ModuleName ModuleDeps)

instance ToJson DepJSON where
    json (DepJSON m) =
        JSObject
            [ (moduleNameString target, module_deps md)
            | (target, md) <- Map.toList m
            ]
      where
        module_deps ModuleDeps{source, boot} =
            JSObject (("boot", maybe JSNull (JSObject . deps) boot) : deps source)

        deps Deps{packages = PackageDeps packages, ..} =
            [ ("sources", array sources (unsafeDecodeUtf . normalise))
            , ("modules", array (fst modules) moduleNameString)
            , ("modules-boot", array (snd modules) moduleNameString)
            ,
                ( "packages"
                , JSArray
                    [ package name unit_id package_id mods
                    | ((name, unit_id, package_id), mods) <- Map.toList packages
                    ]
                )
            , ("cpp", array cpp (unsafeDecodeUtf . normalise))
            , ("options", array options id)
            , ("preprocessor", maybe JSNull JSString preprocessor)
            ]

        package name unit_id (PackageId package_id) mods =
            JSObject
                [ ("id", JSString (unitIdString unit_id))
                , ("name", JSString name)
                , ("package-id", JSString (unpackFS package_id))
                , ("modules", array mods moduleNameString)
                ]

        array values render = JSArray (fmap (JSString . render) (Set.toList values))

initDepJSON :: IO (IORef DepJSON)
initDepJSON = newIORef $ DepJSON Map.empty

insertDepJSON :: ModuleName -> IsBootInterface -> Deps -> DepJSON -> DepJSON
insertDepJSON target is_boot dep (DepJSON m0) =
    DepJSON $ Map.insertWith (<>) target new m0
  where
    new
        | IsBoot <- is_boot = mempty{boot = Just dep}
        | otherwise = mempty{source = dep}

updateDepJSON :: Bool -> Maybe FilePath -> DepNode -> [Dep] -> DepJSON -> DepJSON
updateDepJSON include_pkgs preprocessor DepNode{..} deps =
    insertDepJSON (moduleName dn_mod) dn_boot payload
  where
    payload = node_data <> foldMap dep deps

    node_data =
        mempty
            { sources = Set.singleton dn_src
            , preprocessor
            , options = dn_options
            }

    dep = \case
        DepHi{dep_mod, dep_local, dep_unit, dep_boot}
            | dep_local
            , let set = Set.singleton (moduleName dep_mod)
                  value
                    | IsBoot <- dep_boot = (Set.empty, set)
                    | otherwise = (set, Set.empty) ->
                mempty{modules = value}
            | include_pkgs
            , Just unit <- dep_unit
            , let PackageName nameFS = GHC.unitPackageName unit
                  name = unpackFS nameFS
                  withLibName (PackageName c) = name ++ ":" ++ unpackFS c
                  lname = maybe name withLibName (GHC.unitComponentName unit)
                  key = (lname, GHC.unitId unit, unitPackageId unit) ->
                mempty{packages = PackageDeps (Map.singleton key (Set.singleton (moduleName dep_mod)))}
            | otherwise ->
                mempty
        DepCpp{dep_path_cpp} ->
            mempty{cpp = Set.singleton dep_path_cpp}

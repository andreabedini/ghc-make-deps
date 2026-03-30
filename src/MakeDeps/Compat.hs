{-# LANGUAGE CPP #-}

module MakeDeps.Compat (
    unsafeDecodeUtf,
#if __GLASGOW_HASKELL__ < 912
    ml_hs_file_ospath,
    ml_obj_file_ospath,
    ml_hi_file_ospath,
    msHsFileOsPath,
    msObjFileOsPath,
    msHiFileOsPath,
#endif
) where

#if __GLASGOW_HASKELL__ >= 912
import GHC.Data.OsPath (unsafeDecodeUtf)
#else
import GHC.Data.Maybe (expectJust)
import GHC.Prelude
import GHC.Unit.Finder (ModLocation, ml_hs_file, ml_hi_file, ml_obj_file)
import GHC.Unit.Module.ModSummary (ModSummary, ms_location)
import GHC.Utils.Misc (HasCallStack)
import GHC.Utils.Panic (panic)
import System.OsPath (OsPath, OsString, decodeUtf, unsafeEncodeUtf)

unsafeDecodeUtf :: HasCallStack => OsPath -> FilePath
unsafeDecodeUtf p = either (\err -> panic $ "Failed to decodeUtf \"" ++ show p ++ "\", because: " ++ show err) id (decodeUtf p)

ml_hs_file_ospath :: ModLocation -> Maybe OsString
ml_hs_file_ospath m = unsafeEncodeUtf <$> ml_hs_file m

ml_obj_file_ospath, ml_hi_file_ospath :: ModLocation -> OsString
ml_obj_file_ospath = unsafeEncodeUtf . ml_obj_file
ml_hi_file_ospath = unsafeEncodeUtf . ml_hi_file

msHsFileOsPath, msObjFileOsPath, msHiFileOsPath :: ModSummary -> OsString
msHsFileOsPath ms = expectJust "msHsFileOsPath" (ml_hs_file_ospath (ms_location ms))
msObjFileOsPath ms = ml_obj_file_ospath (ms_location ms)
msHiFileOsPath ms = ml_hi_file_ospath (ms_location ms)
#endif

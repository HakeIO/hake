module Development.Hake.Actions () where

import Development.Hake.Types
import Development.Shake
import Development.Shake.FilePath

initEnvironment
  :: (FilePath, FilePath)
  -> InitOptions
  -> Maybe (Action PersistedEnvironment)
initEnvironment (rootDir, _) sm = Just action where
  action = do
    let penv = PersistedEnvironment
          { penvRootDirectory    = rootDir
          , penvBuildDirectory   = rootDir </> "build"
          , penvPrefixDirectory  = pfx
          , penvPkgConfDirectory = rootDir </> "build" </> "package.conf.d"
          , penvAdditionalPkgConfDirectories = ioAdditionalPackageDbs sm
          }

        pfx | InitOptions{ioDesiredPrefix = prefix} <- sm = rootDir </> prefix
            | otherwise = rootDir </> "build" </> "dist"

    return $! penv

{-# LANGUAGE RecordWildCards #-}
module Development.Hake.Types (
    hakeParser
  , HakeMode(..)
  , InitOptions(..)
  , PersistedEnvironment(..)) where

import Control.DeepSeq
import Control.Monad (liftM5)
import Data.Binary
import Data.Hashable
import Data.Typeable
import Development.Shake.FilePath
import Options.Applicative

data Verbosity = Quiet | Verbose deriving (Enum,Read,Show)

data InitOptions = InitOptions {
  ioDesiredPrefix    :: FilePath,
  ioKeepGoing :: Bool,
  ioAdditionalPackageDbs :: [FilePath]
  }

data HakeMode = Init InitOptions

data HakeOptions = HakeOptions {
    hoVerbosity :: Verbosity,
    hoMode :: HakeMode
  }

verbosityParser :: Parser Verbosity
verbosityParser = option auto ( short 'v' <> long "verbose" <> value Quiet <> help "Desired verbosity level")

keepGoingParser :: Parser Bool
keepGoingParser = switch (short 'k' <> long "keep-going" <> help "Continue as much as possible after an error")

hakeModeParser :: Parser HakeMode
hakeModeParser = hsubparser $
  command "init" (info (Init <$> (InitOptions
   <$> option str (value ("dist" </> "build") <> long "prefix" <> help "Installation prefix")
   <*> keepGoingParser
   <*> many (option str (long "package-db" <> help "Additional Package DBs for finding dependencies."))
     )) $ progDesc "init")

hakeParser :: ParserInfo HakeOptions
hakeParser = info ((HakeOptions <$> verbosityParser <*> hakeModeParser) <**> helper)
                     (progDesc "The hake program" <> fullDesc)

data PersistedEnvironment = PersistedEnvironment
  { penvRootDirectory    :: FilePath
  , penvBuildDirectory   :: FilePath
  , penvPrefixDirectory  :: FilePath
  , penvPkgConfDirectory :: FilePath
  , penvAdditionalPkgConfDirectories :: [FilePath]
  } deriving (Show, Eq, Ord, Typeable)

instance Binary PersistedEnvironment where
  put PersistedEnvironment{..} = do
    put penvRootDirectory
    put penvBuildDirectory
    put penvPrefixDirectory
    put penvPkgConfDirectory
    put penvAdditionalPkgConfDirectories

  get =
    liftM5
      PersistedEnvironment
        get
        get
        get
        get
        get

instance Hashable PersistedEnvironment where
  hashWithSalt s PersistedEnvironment{..} = hashWithSalt s
    ( penvRootDirectory
    , penvBuildDirectory
    , penvPrefixDirectory
    , penvPkgConfDirectory
    , penvAdditionalPkgConfDirectories)

instance NFData PersistedEnvironment where
  rnf PersistedEnvironment{..} =
    rnf penvRootDirectory `seq`
    rnf penvBuildDirectory `seq`
    rnf penvPrefixDirectory `seq`
    rnf penvPkgConfDirectory `seq`
    rnf penvAdditionalPkgConfDirectories `seq`
    ()

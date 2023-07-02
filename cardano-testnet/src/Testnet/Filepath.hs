{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Testnet.Filepath
  ( TmpAbsolutePath(..)
  , makeDbDir
  , makeLogDir
  , makeSocketDir
  , makeSprocket
  , makeTmpBaseAbsPath
  ) where

import           Prelude

import           Data.String (IsString (..))
import           System.FilePath

import           Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))




makeSprocket
  :: TmpAbsolutePath
  -> String -- ^ node name
  -> Sprocket
makeSprocket tmpAbsPath node
  = Sprocket (makeTmpBaseAbsPath tmpAbsPath) (makeSocketDir tmpAbsPath </> node)

-- Temporary path used at runtime
newtype TmpAbsolutePath = TmpAbsolutePath
  { unTmpAbsPath :: FilePath
  } deriving (Eq, Show, IsString)

makeTmpRelPath :: TmpAbsolutePath -> FilePath
makeTmpRelPath (TmpAbsolutePath fp) = makeRelative (makeTmpBaseAbsPath (TmpAbsolutePath fp)) fp

makeSocketDir :: TmpAbsolutePath -> FilePath
makeSocketDir fp = makeTmpRelPath fp </> "socket"

makeTmpBaseAbsPath :: TmpAbsolutePath -> FilePath
makeTmpBaseAbsPath (TmpAbsolutePath fp) = takeDirectory fp

makeLogDir :: TmpAbsolutePath -> FilePath
makeLogDir (TmpAbsolutePath fp) = fp </> "logs"

makeDbDir :: Int -> TmpAbsolutePath -> FilePath
makeDbDir nodeNumber (TmpAbsolutePath fp) = fp </> "db/node-" </> show nodeNumber

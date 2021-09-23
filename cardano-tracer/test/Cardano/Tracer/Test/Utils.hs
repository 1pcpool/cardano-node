{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Tracer.Test.Utils
  ( false
  , propRunInLogsStructure
  ) where

import           Control.Exception (finally)
import           System.Directory
import           System.IO.Extra
import           Test.Tasty.QuickCheck

false :: String -> IO Property
false msg = return . counterexample msg $ property False

propRunInLogsStructure
  :: (FilePath -> FilePath -> IO Property)
  -> Property
propRunInLogsStructure testAction = ioProperty $ do
  tmpDir <- getTemporaryDirectory
  (rootDir, deleteDir) <- newTempDirWithin tmpDir
  (localSock, deleteSock) <- newTempFileWithin tmpDir
  testAction rootDir localSock `finally` deleteSock >> deleteDir

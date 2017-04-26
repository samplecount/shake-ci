-- Copyright 2017 Samplecount S.L.
--
-- All rights reserved.
module Development.Shake.CI.Env (
    getEnv'
  , withEnv
  , withEnvIO
) where

import           Development.Shake
import qualified System.Environment as Env

getEnv' :: String -> Action String
getEnv' name =
  getEnvWithDefault
    (error $ "Environment variable " ++ name ++ " is undefined")
    name

withEnv :: String -> (String -> Action ()) -> Action ()
withEnv s a = maybe (return ()) a =<< getEnv s

withEnvIO :: String -> (String -> IO ()) -> IO ()
withEnvIO s a = maybe (return ()) a =<< Env.lookupEnv s

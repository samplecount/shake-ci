-- Copyright 2017 Samplecount S.L.
--
-- All rights reserved.
module Development.Shake.CI.Env (
    getEnv'
  , withEnv
  , withEnvIO
  , isCI
  , whenCI
  , unlessCI
) where

import           Control.Monad (unless, when)
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

isCI :: Action Bool
isCI = maybe False (const True) <$> getEnv "JENKINS_URL"

whenCI :: Action () -> Action ()
whenCI a = isCI >>= flip when a

unlessCI :: Action () -> Action ()
unlessCI a = isCI >>= flip unless a

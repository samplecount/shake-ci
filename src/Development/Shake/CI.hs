-- Copyright 2017 Samplecount S.L.
--
-- All rights reserved.

module Development.Shake.CI (
    getEnv'
  , withEnv
  , isCI
  , whenCI
  , unlessCI
  , slack
) where

import Development.Shake.CI.Env
import Development.Shake.CI.Slack

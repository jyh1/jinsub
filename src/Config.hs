{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Config where

import Data.Text
import Text.RawString.QQ

defaultPBS :: Text
defaultPBS = [r|
#!/bin/sh
# An example template file.

#PBS -N job
#PBS -o job.log
#PBS -e job.err
#PBS -q job_run
#PBS -l nodes=1:ppn=1
#PBS -l walltime=24:00:00


cd "#(WorkingDirectory)"

echo BEGIN EXECUTING
# command placeholder.
#(CMD)
|]

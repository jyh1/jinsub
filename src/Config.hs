{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Config where

import Data.Text
import Text.RawString.QQ

defaultPBS :: Text
defaultPBS = [r|
#!/bin/sh
# An example template file of jinsub.

#PBS -N job_name
#PBS -o job.log
#PBS -e job.err
#PBS -q job_run
#PBS -l walltime=02:00:00

# Definitions of environment variable will be expanded here.
#DEFS

cd $CurrentDirectory

echo begin executing
# command variable.
$CMD
|]

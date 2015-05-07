module OhBool.Reducer where

import OhBool.Common
import OhBool.Evaluation
import OhBool.Utils

import qualified Data.Map as M

reduce :: Expression -> Expression
reduce ex = ex
  where table = constructTruthTable ex
        eval = M.toAscList $ evaluation table
        grayEval = grayify eval
        trues = filter snd grayEval


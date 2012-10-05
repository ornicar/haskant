import           Test.Framework                       (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)

import           Test.HUnit
import           Test.QuickCheck

import           Data.Time.Calendar
import           Data.Time.Clock

import           Debug.Trace

import           Ants
import           Protocol

tests = [
          testGroup "properties" $ zipWith (testProperty . show) [1::Int ..] $
            [ property True]
        ]

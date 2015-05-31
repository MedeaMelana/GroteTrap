module Main where

import Test.QuickCheck
import Text.Printf

import Language.GroteTrap.Range
import Language.GroteTrap.Trees

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

tests :: [(String, IO ())]
tests  = [ ("range/propUnionRange", test propUnionRange)
         , ("range/propDistRange", test propDistRange)
         , ("range/propDistRangeComm", test propDistRangeComm)
         , ("trees/propDownUp", test propDownUp)
         , ("trees/propUpDown", test propUpDown)
         , ("trees/propLeftRight", test propLeftRight)
         , ("trees/propRightLeft", test propRightLeft)
         ]

-- ranges

propUnionRange r1 r2 = validRange r1 && validRange r2 ==> u `includes` r1 && u `includes` r2
  where u = unionRange r1 r2

propDistRange r1 r2 = validRange r1 && validRange r2 && r1 `includes` r2 ==>
                        distRange r1 r2 <= size r1

propDistRangeComm r1 r2 = distRange r1 r2 == distRange r2 r1

-- tree paths

propDownUp     path  =  up (down path) == path
propUpDown     path  =  not (null path) ==> init (down (up path)) == init path
propLeftRight  path  =  not (null path) && last path > 0 ==> right (left path) == path
propRightLeft  path  =  not (null path) && last path >= 0 ==> left (right path) == path

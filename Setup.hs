-- @+leo-ver=4-thin
-- @+node:gcross.20091208183517.1472:@thin Setup.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091208183517.1473:<< Language extensions >>
{-# LANGUAGE PackageImports #-}
-- @-node:gcross.20091208183517.1473:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20091208183517.1474:<< Import needed modules >>
import Blueprint.Tools.GHC.Main
-- @-node:gcross.20091208183517.1474:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091208183517.1475:main
main = simpleDefaultMain
    [("","sources")]
    (Just
        ([("","tests")]
        ,[]
        ,["HUnit == 1.*"
         ,"QuickCheck == 2.*"
         ,"test-framework == 0.2.*"
         ,"test-framework-hunit == 0.2.*"
         ,"test-framework-quickcheck2 == 0.2.*"
         ,"random == 1.*"
         ]
        )
    )
    ["-O2","-fvia-C","-optc=-O3"]
-- @-node:gcross.20091208183517.1475:main
-- @-others
-- @-node:gcross.20091208183517.1472:@thin Setup.hs
-- @-leo

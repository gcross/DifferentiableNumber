-- @+leo-ver=4-thin
-- @+node:gcross.20091208183517.1524:@thin test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091208183517.1525:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
-- @-node:gcross.20091208183517.1525:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20091208183517.1526:<< Import needed modules >>
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Data.Differentiable
import Data.Differentiable.FunctionExpansion
import Data.Differentiable.Number
import Data.Differentiable.Quantum3D
-- @-node:gcross.20091208183517.1526:<< Import needed modules >>
-- @nl

-- @+others
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20091208183517.1531:<< Tests >>
    -- @+others
    -- @+node:gcross.20091208183517.1577:Total derivatives
    [testGroup "Total derivatives"
        -- @    @+others
        -- @+node:gcross.20091208183517.1579:Constant rule
        [testProperty "Constant rule" $
            \(value :: Integer) (i :: Coordinate) -> d i (constant value) == 0
        -- @-node:gcross.20091208183517.1579:Constant rule
        -- @+node:gcross.20091208183517.1585:Variable rule
        ,testProperty "Variable rule" $
            \(value :: Integer) (i :: Coordinate) (j :: Coordinate) -> d i (variable j value) == if i == j then 1 else 0
        -- @-node:gcross.20091208183517.1585:Variable rule
        -- @+node:gcross.20091208183517.1580:Sum rule
        ,testProperty "Sum rule" $
            \(f :: FunctionExpansion Integer) (g :: FunctionExpansion Integer) (i :: Coordinate) -> d i (f+g) == d i f + d i g
        -- @-node:gcross.20091208183517.1580:Sum rule
        -- @+node:gcross.20091208183517.1581:Product rule
        ,testProperty "Product rule" $
            \(f :: FunctionExpansion Integer) (g :: FunctionExpansion Integer)  (i :: Coordinate) -> d i (f*g) == d i f * g + f * d i g
        -- @-node:gcross.20091208183517.1581:Product rule
        -- @-others
        ]
    -- @-node:gcross.20091208183517.1577:Total derivatives
    -- @-others
    -- @-node:gcross.20091208183517.1531:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091208183517.1524:@thin test.hs
-- @-leo

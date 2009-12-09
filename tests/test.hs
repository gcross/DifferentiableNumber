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
import Data.Complex

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Data.Differentiable
import Data.Differentiable.FunctionExpansion
import Data.Differentiable.Number
import Data.Differentiable.Quantum3D
import Data.Differentiable.Testing
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
        -- @+node:gcross.20091209122152.1330:Exchange rule
        ,testProperty "Exchange rule" $
            \(i :: Coordinate)
             (j :: Coordinate)
             -> d i ~~ d j === (0 :: Integer) *| id
        -- @-node:gcross.20091209122152.1330:Exchange rule
        -- @-others
        ]
    -- @-node:gcross.20091208183517.1577:Total derivatives
    -- @+node:gcross.20091209122152.1325:Commutators
    ,testGroup "Commutators"
        -- @    @+others
        -- @+node:gcross.20091209122152.1327:[r_i,r_j]
        [testProperty "[r_i,r_j] == 0" $
            \(i :: Coordinate)
             (j :: Coordinate)
             -> r_ i ~~ r_ j === (0 :: Integer) *| id
        -- @-node:gcross.20091209122152.1327:[r_i,r_j]
        -- @+node:gcross.20091209122152.1458:[p_i,p_j]
        ,testProperty "[p_i,p_j] == 0" $
            \(i :: Coordinate)
             (j :: Coordinate)
             -> p_ i . p_ j =~= (p_ j . p_ i :: DifferentialOperator (Complex Double))
        -- @-node:gcross.20091209122152.1458:[p_i,p_j]
        -- @+node:gcross.20091209122152.1477:[r_i,dj]
        ,testProperty "[r_j,dk] == delta_jk" $
            \(j :: Coordinate)
             (k :: Coordinate)
             -> d j ~~ r_ k === if j == k then id else (0 :: Integer) *| id
        -- @-node:gcross.20091209122152.1477:[r_i,dj]
        -- @+node:gcross.20091209122152.1472:[r_i,r_j]
        ,testProperty "[r_j,p_k] == i delta_jk" $
            \(j :: Coordinate)
             (k :: Coordinate)
             -> if j == k
                then r_ j ~~ p_ j =~= (i :: Complex Double) *| id
                else r_ j . p_ k =~= (p_ k . r_ j :: DifferentialOperator (Complex Double))
        -- @-node:gcross.20091209122152.1472:[r_i,r_j]
        -- @-others
        ]
    -- @-node:gcross.20091209122152.1325:Commutators
    -- @-others
    -- @-node:gcross.20091208183517.1531:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091208183517.1524:@thin test.hs
-- @-leo

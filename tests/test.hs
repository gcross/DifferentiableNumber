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
-- @nonl
-- @-node:gcross.20091208183517.1526:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091212141130.1606:Helpers
-- @+node:gcross.20091212141130.1607:intFnType
intFnType :: ((Integer,Integer,Integer) -> FunctionExpansion Integer) -> ((Integer,Integer,Integer) -> FunctionExpansion Integer)
intFnType = id
-- @-node:gcross.20091212141130.1607:intFnType
-- @+node:gcross.20091212141130.1611:doubleFnType
doubleFnType ::
    ((Double,Double,Double) -> FunctionExpansion Double) ->
    ((Double,Double,Double) -> FunctionExpansion Double)
doubleFnType = id
-- @-node:gcross.20091212141130.1611:doubleFnType
-- @-node:gcross.20091212141130.1606:Helpers
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20091208183517.1531:<< Tests >>
    -- @+others
    -- @+node:gcross.20091208183517.1577:Total derivatives
    [testGroup "Total derivatives"
        -- @    @+others
        -- @+node:gcross.20091212141130.1422:Identities
        [testGroup "Identities"
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
            -- @+node:gcross.20091212141130.1432:Quotient rule
            ,testProperty "Quotient rule" $
                \(f :: FunctionExpansion Rational) (g :: FunctionExpansion Rational)  (i :: Coordinate) ->
                    g > 0 ==> d i (f/g) == (d i f * g - f * d i g) / (g*g)
            -- @-node:gcross.20091212141130.1432:Quotient rule
            -- @+node:gcross.20091209122152.1330:Exchange rule
            ,testProperty "Exchange rule" $
                \(i :: Coordinate)
                 (j :: Coordinate)
                 -> d i ~~ d j === (0 :: Integer) *| id
            -- @-node:gcross.20091209122152.1330:Exchange rule
            -- @-others
            ]
        -- @-node:gcross.20091212141130.1422:Identities
        -- @+node:gcross.20091212141130.1421:Functions
        ,testGroup "Functions"
            -- @    @+others
            -- @+node:gcross.20091212141130.1424:exp
            [testProperty "exp" $ mapSize (const 1) $
                \argument@((value :: Double) ::> _) (i :: Coordinate) ->
                    d i (exp argument) ~= constant (exp value) * d i argument
            -- @-node:gcross.20091212141130.1424:exp
            -- @+node:gcross.20091212141130.1426:sin
            ,testProperty "sin" $ mapSize (const 1) $
                \argument@((value :: Double) ::> _) (i :: Coordinate) ->
                    d i (sin argument) ~= constant (cos value) * d i argument
            -- @-node:gcross.20091212141130.1426:sin
            -- @+node:gcross.20091212141130.1428:cos
            ,testProperty "cos" $ mapSize (const 1) $
                \argument@((value :: Double) ::> _) (i :: Coordinate) ->
                    d i (cos argument) ~= constant (-sin value) * d i argument
            -- @-node:gcross.20091212141130.1428:cos
            -- @+node:gcross.20091212141130.1430:sqrt
            ,testProperty "sqrt" $ mapSize (const 1) $
                \argument@((value :: Double) ::> _) (i :: Coordinate) ->
                    value > 0 ==>
                        (d i (sqrt argument) ~= constant ((0.5 *) . recip . sqrt $ value) * d i argument)
            -- @-node:gcross.20091212141130.1430:sqrt
            -- @-others
            ]
        -- @-node:gcross.20091212141130.1421:Functions
        -- @+node:gcross.20091212141130.1594:Examples
        ,testGroup "Examples"
            -- @    @+others
            -- @+node:gcross.20091212141130.1595:x^2
            [testProperty "x^2" $
                d X (v_ X * v_ X) === intFnType 2 * v_ X
            -- @-node:gcross.20091212141130.1595:x^2
            -- @+node:gcross.20091212141130.1613:sin x * cos x
            ,testProperty "sin x * cos x" $
                d X (sin (v_ X) * cos (v_ X)) =~= doubleFnType (cos (2 * (v_ X)))
            -- @-node:gcross.20091212141130.1613:sin x * cos x
            -- @-others
            ]
        -- @-node:gcross.20091212141130.1594:Examples
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

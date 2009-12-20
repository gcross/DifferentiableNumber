-- @+leo-ver=4-thin
-- @+node:gcross.20091220080702.1924:@thin MultiParticle.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091220080702.1925:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
-- @-node:gcross.20091220080702.1925:<< Language extensions >>
-- @nl

module MultiParticle where

-- @<< Import needed modules >>
-- @+node:gcross.20091220080702.1926:<< Import needed modules >>
import Data.Complex

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Data.Differentiable
import Data.Differentiable.FunctionExpansion
import Data.Differentiable.Number
import Data.Differentiable.Testing
import Data.Differentiable.Quantum
import Data.Differentiable.Quantum.MultiParticle
-- @-node:gcross.20091220080702.1926:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091220080702.1927:Helpers
-- @+node:gcross.20091220080702.1928:intFnType
intFnType :: ((Integer,Integer,Integer) -> FunctionExpansion Integer) -> ((Integer,Integer,Integer) -> FunctionExpansion Integer)
intFnType = id
-- @-node:gcross.20091220080702.1928:intFnType
-- @+node:gcross.20091220080702.1929:doubleFnType
doubleFnType ::
    ((Double,Double,Double) -> FunctionExpansion Double) ->
    ((Double,Double,Double) -> FunctionExpansion Double)
doubleFnType = id
-- @-node:gcross.20091220080702.1929:doubleFnType
-- @-node:gcross.20091220080702.1927:Helpers
-- @+node:gcross.20091220080702.1930:Tests
tests =
    -- @    @+others
    -- @+node:gcross.20091220080702.1931:Total derivatives
    [testGroup "Total derivatives"
        -- @    @+others
        -- @+node:gcross.20091220080702.1932:Identities
        [testGroup "Identities"
            -- @    @+others
            -- @+node:gcross.20091220080702.1933:Constant rule
            [testProperty "Constant rule" $
                \(value :: Integer) (i :: Coordinate) -> d i (fromInteger value) == (0 :: DifferentiableNumber Integer) 
            -- @-node:gcross.20091220080702.1933:Constant rule
            -- @+node:gcross.20091220080702.1934:Variable rule
            ,testProperty "Variable rule" $
                \(value :: Integer) (i :: Coordinate) (j :: Coordinate) -> d i (variable j value) == if i == j then 1 else 0
            -- @-node:gcross.20091220080702.1934:Variable rule
            -- @+node:gcross.20091220080702.1935:Sum rule
            ,testProperty "Sum rule" $
                \(f :: FunctionExpansion Integer) (g :: FunctionExpansion Integer) (i :: Coordinate) -> d i (f+g) == d i f + d i g
            -- @-node:gcross.20091220080702.1935:Sum rule
            -- @+node:gcross.20091220080702.1936:Product rule
            ,testProperty "Product rule" $
                \(f :: FunctionExpansion Integer) (g :: FunctionExpansion Integer)  (i :: Coordinate) -> d i (f*g) == d i f * g + f * d i g
            -- @-node:gcross.20091220080702.1936:Product rule
            -- @+node:gcross.20091220080702.1937:Quotient rule
            ,testProperty "Quotient rule" $
                \(f :: FunctionExpansion Rational) (g :: FunctionExpansion Rational)  (i :: Coordinate) ->
                    g > 0 ==> d i (f/g) == (d i f * g - f * d i g) / (g*g)
            -- @-node:gcross.20091220080702.1937:Quotient rule
            -- @+node:gcross.20091220080702.1938:Exchange rule
            ,testProperty "Exchange rule" $
                \(i :: Coordinate)
                 (j :: Coordinate)
                 -> d i ~~ d j === (0 :: Integer) *| id
            -- @-node:gcross.20091220080702.1938:Exchange rule
            -- @-others
            ]
        -- @-node:gcross.20091220080702.1932:Identities
        -- @+node:gcross.20091220080702.1939:Functions
        ,testGroup "Functions"
            -- @    @+others
            -- @+node:gcross.20091220080702.1940:exp
            [testProperty "exp" $ mapSize (const 1) $
                \argument@((value :: Double) ::> _) (i :: Coordinate) ->
                    d i (exp argument) ~= constant (exp value) * d i argument
            -- @-node:gcross.20091220080702.1940:exp
            -- @+node:gcross.20091220080702.1941:sin
            ,testProperty "sin" $ mapSize (const 1) $
                \argument@((value :: Double) ::> _) (i :: Coordinate) ->
                    d i (sin argument) ~= constant (cos value) * d i argument
            -- @-node:gcross.20091220080702.1941:sin
            -- @+node:gcross.20091220080702.1942:cos
            ,testProperty "cos" $ mapSize (const 1) $
                \argument@((value :: Double) ::> _) (i :: Coordinate) ->
                    d i (cos argument) ~= constant (-sin value) * d i argument
            -- @-node:gcross.20091220080702.1942:cos
            -- @+node:gcross.20091220080702.1943:sqrt
            ,testProperty "sqrt" $ mapSize (const 1) $
                \argument@((value :: Double) ::> _) (i :: Coordinate) ->
                    value > 0 ==>
                        (d i (sqrt argument) ~= constant ((0.5 *) . recip . sqrt $ value) * d i argument)
            -- @-node:gcross.20091220080702.1943:sqrt
            -- @-others
            ]
        -- @-node:gcross.20091220080702.1939:Functions
        -- @-others
        ]
    -- @-node:gcross.20091220080702.1931:Total derivatives
    -- @+node:gcross.20091220080702.1947:Commutators
    ,testGroup "Commutators"
        -- @    @+others
        -- @+node:gcross.20091220080702.1948:[r_i,r_j]
        [testProperty "[r_i,r_j] == 0" $
            \(i :: Coordinate)
             (j :: Coordinate)
             -> r_ i ~~ r_ j === (0 :: Integer) *| id
        -- @-node:gcross.20091220080702.1948:[r_i,r_j]
        -- @+node:gcross.20091220080702.1949:[p_i,p_j]
        ,testProperty "[p_i,p_j] == 0" $
            \(i :: Coordinate)
             (j :: Coordinate)
             -> p_ i . p_ j =~= (p_ j . p_ i :: DifferentialOperator (Complex Double))
        -- @-node:gcross.20091220080702.1949:[p_i,p_j]
        -- @+node:gcross.20091220080702.1950:[r_i,dj]
        ,testProperty "[r_j,dk] == delta_jk" $
            \(j :: Coordinate)
             (k :: Coordinate)
             -> d j ~~ r_ k === if j == k then id else (0 :: Integer) *| id
        -- @-node:gcross.20091220080702.1950:[r_i,dj]
        -- @+node:gcross.20091220080702.1951:[r_i,r_j]
        ,testProperty "[r_j,p_k] == i delta_jk" $
            \(j :: Coordinate)
             (k :: Coordinate)
             -> if j == k
                then r_ j ~~ p_ j =~= (i :: Complex Double) *| id
                else r_ j . p_ k =~= (p_ k . r_ j :: DifferentialOperator (Complex Double))
        -- @-node:gcross.20091220080702.1951:[r_i,r_j]
        -- @-others
        ]
    -- @-node:gcross.20091220080702.1947:Commutators
    -- @-others
    ]
-- @-node:gcross.20091220080702.1930:Tests
-- @-others

-- @-node:gcross.20091220080702.1924:@thin MultiParticle.hs
-- @-leo

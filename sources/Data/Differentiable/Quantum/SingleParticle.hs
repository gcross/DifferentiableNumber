-- @+leo-ver=4-thin
-- @+node:gcross.20091220080702.1787:@thin SingleParticle.hs
-- @@language Haskell

module Data.Differentiable.Quantum.SingleParticle where

-- @<< Import needed modules >>
-- @+node:gcross.20091220080702.1788:<< Import needed modules >>
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import System.Random

import Data.Differentiable
import Data.Differentiable.FunctionExpansion
import Data.Differentiable.Number
import Data.Differentiable.Quantum
-- @-node:gcross.20091220080702.1788:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091220080702.1789:Types
-- @+node:gcross.20091220080702.1790:Coordinate
type Coordinate = XYZ
-- @-node:gcross.20091220080702.1790:Coordinate
-- @-node:gcross.20091220080702.1789:Types
-- @+node:gcross.20091220080702.1799:Functions
-- @+node:gcross.20091220080702.1800:v_
v_ :: Num a => Coordinate -> (a,a,a) -> FunctionExpansion a
v_ coordinate argument@(vx,vy,vz) = [vx,vy,vz] :-> 
    case coordinate of
        X -> vx ::> [one,zero,zero]
        Y -> vy ::> [zero,one,zero]
        Z -> vz ::> [zero,zero,one]
-- @-node:gcross.20091220080702.1800:v_
-- @-node:gcross.20091220080702.1799:Functions
-- @-others

-- @-node:gcross.20091220080702.1787:@thin SingleParticle.hs
-- @-leo

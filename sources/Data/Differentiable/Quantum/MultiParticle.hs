-- @+leo-ver=4-thin
-- @+node:gcross.20091220080702.1815:@thin MultiParticle.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091220080702.1952:<< Language extensions >>
{-# LANGUAGE TypeSynonymInstances #-}
-- @-node:gcross.20091220080702.1952:<< Language extensions >>
-- @nl

module Data.Differentiable.Quantum.MultiParticle where

-- @<< Import needed modules >>
-- @+node:gcross.20091220080702.1816:<< Import needed modules >>
import Control.Applicative.Infix
import Control.Monad

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import System.Random

import Data.Differentiable
import Data.Differentiable.FunctionExpansion
import Data.Differentiable.Number
import Data.Differentiable.Quantum
import qualified Data.Differentiable.Quantum.SingleParticle as SP
-- @-node:gcross.20091220080702.1816:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091220080702.1817:Types
-- @+node:gcross.20091220080702.1818:Coordinate
data Coordinate = Int :> SP.Coordinate deriving (Show,Eq)
-- @-node:gcross.20091220080702.1818:Coordinate
-- @-node:gcross.20091220080702.1817:Types
-- @+node:gcross.20091220080702.1823:Instances
-- @+node:gcross.20091220080702.1829:Enum Coordinate
instance Enum Coordinate where
    toEnum = (`div` 3) <^(:>)^> toEnum . (`mod` 3)
    fromEnum (index :> coordinate) = index*3 + fromEnum coordinate
-- @-node:gcross.20091220080702.1829:Enum Coordinate
-- @-node:gcross.20091220080702.1823:Instances
-- @+node:gcross.20091220080702.1830:Generators
-- @+node:gcross.20091220080702.1824:Coordinate
instance Arbitrary Coordinate where
    arbitrary = liftM2 (:>) (fmap abs $ resize 5 arbitrary) (elements [SP.X,SP.Y,SP.Z])
-- @-node:gcross.20091220080702.1824:Coordinate
-- @-node:gcross.20091220080702.1830:Generators
-- @-others

-- @-node:gcross.20091220080702.1815:@thin MultiParticle.hs
-- @-leo

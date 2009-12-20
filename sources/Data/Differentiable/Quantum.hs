-- @+leo-ver=4-thin
-- @+node:gcross.20091220080702.1831:@thin Quantum.hs
-- @@language Haskell

module Data.Differentiable.Quantum where

-- @<< Import needed modules >>
-- @+node:gcross.20091220080702.1833:<< Import needed modules >>
import Data.Complex

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import System.Random

import Data.Differentiable
import Data.Differentiable.FunctionExpansion
import Data.Differentiable.Number
-- @-node:gcross.20091220080702.1833:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091220080702.1837:Quantum Operators
-- @+node:gcross.20091220080702.1838:r_
r_ :: (Enum i, Num a) => i -> DifferentialOperator a
r_ = multiplyByCoordinate
-- @-node:gcross.20091220080702.1838:r_
-- @+node:gcross.20091220080702.1839:p_
p_ :: (Enum i, RealFloat a) => i -> DifferentialOperator (Complex a)
p_ k = (-i) *| d k
-- @-node:gcross.20091220080702.1839:p_
-- @-node:gcross.20091220080702.1837:Quantum Operators
-- @+node:gcross.20091220080702.1961:Types
-- @+node:gcross.20091220080702.2343:XYZ
data XYZ = X | Y | Z deriving (Show,Eq,Enum)
-- @nonl
-- @-node:gcross.20091220080702.2343:XYZ
-- @-node:gcross.20091220080702.1961:Types
-- @+node:gcross.20091220080702.1957:Generators
-- @+node:gcross.20091220080702.2345:XYZ
instance Arbitrary XYZ where
    arbitrary = elements [X,Y,Z]
-- @-node:gcross.20091220080702.2345:XYZ
-- @-node:gcross.20091220080702.1957:Generators
-- @-others
-- @-node:gcross.20091220080702.1831:@thin Quantum.hs
-- @-leo

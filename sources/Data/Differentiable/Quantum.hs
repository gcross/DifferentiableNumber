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
-- @+node:gcross.20091220080702.1963:Position
newtype Position a = Position [a]
-- @-node:gcross.20091220080702.1963:Position
-- @-node:gcross.20091220080702.1961:Types
-- @+node:gcross.20091220080702.1957:Generators
-- @+node:gcross.20091220080702.1959:Position
instance Random a => Arbitrary (Position a) where
    arbitrary = MkGen $ \stdgen size -> Position (randoms stdgen)
-- @-node:gcross.20091220080702.1959:Position
-- @+node:gcross.20091220080702.1960:FunctionExpansion
instance (Num a, Random a, Arbitrary a) => Arbitrary (FunctionExpansion a) where
    arbitrary = MkGen $ \gen size ->
        let (gen1,gen2) = split gen
            (Position argument) = unGen arbitrary gen1 size
            expansion = unGen arbitrary gen2 size
        in argument :-> expansion
-- @-node:gcross.20091220080702.1960:FunctionExpansion
-- @-node:gcross.20091220080702.1957:Generators
-- @-others
-- @-node:gcross.20091220080702.1831:@thin Quantum.hs
-- @-leo

-- @+leo-ver=4-thin
-- @+node:gcross.20091208183517.1432:@thin Quantum3D.hs
-- @@language Haskell

module Data.Differentiable.Quantum3D where

-- @<< Import needed modules >>
-- @+node:gcross.20091208183517.1447:<< Import needed modules >>
import Data.Tree

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import System.Random

import Data.Differentiable
import Data.Differentiable.FunctionExpansion
import Data.Differentiable.Number
-- @-node:gcross.20091208183517.1447:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091208183517.1560:Types
-- @+node:gcross.20091208183517.1561:Coordinate
data Coordinate = X | Y | Z deriving (Eq,Show,Enum)
-- @-node:gcross.20091208183517.1561:Coordinate
-- @+node:gcross.20091208183517.1583:Position
newtype Position a = Position [a] deriving (Eq,Show)
-- @-node:gcross.20091208183517.1583:Position
-- @-node:gcross.20091208183517.1560:Types
-- @+node:gcross.20091208183517.1459:Operators
-- @+node:gcross.20091208183517.1460:x, y, z
x = multiplyByCoordinate 0
y = multiplyByCoordinate 1
z = multiplyByCoordinate 2
-- @-node:gcross.20091208183517.1460:x, y, z
-- @+node:gcross.20091208183517.1461:px, py, pz
px, py, pz :: DifferentialOperator a
px = d 0
py = d 1
pz = d 2
-- @-node:gcross.20091208183517.1461:px, py, pz
-- @-node:gcross.20091208183517.1459:Operators
-- @+node:gcross.20091208183517.1555:Instances
-- @+node:gcross.20091208183517.1582:Arbitrary Coordinate
instance Arbitrary Coordinate where
    arbitrary = elements [X,Y,Z]
-- @-node:gcross.20091208183517.1582:Arbitrary Coordinate
-- @+node:gcross.20091208183517.1584:Arbitrary Position
instance Arbitrary a => Arbitrary (Position a) where
    arbitrary = fmap Position (vectorOf 3 arbitrary)
-- @-node:gcross.20091208183517.1584:Arbitrary Position
-- @+node:gcross.20091208183517.1569:Arbitrary (FunctionExpansion a)
instance (Num a, Random a, Arbitrary a) => Arbitrary (FunctionExpansion a) where
    arbitrary = MkGen $ \gen size ->
        let (gen1,gen2) = split gen
            (Position argument) = unGen arbitrary gen1 size
            expansion = unGen arbitrary gen2 size
        in argument :-> expansion
-- @-node:gcross.20091208183517.1569:Arbitrary (FunctionExpansion a)
-- @-node:gcross.20091208183517.1555:Instances
-- @-others

-- @-node:gcross.20091208183517.1432:@thin Quantum3D.hs
-- @-leo

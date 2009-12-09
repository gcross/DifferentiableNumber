-- @+leo-ver=4-thin
-- @+node:gcross.20091208183517.1432:@thin Quantum3D.hs
-- @@language Haskell

module Data.Differentiable.Quantum3D where

-- @<< Import needed modules >>
-- @+node:gcross.20091208183517.1447:<< Import needed modules >>
import Data.Complex

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
-- @+node:gcross.20091209122152.1332:Values
-- @+node:gcross.20091209122152.1333:i
i :: RealFloat a => Complex a
i = 0 :+ 1
-- @-node:gcross.20091209122152.1333:i
-- @-node:gcross.20091209122152.1332:Values
-- @+node:gcross.20091208183517.1560:Types
-- @+node:gcross.20091208183517.1561:Coordinate
data Coordinate = X | Y | Z deriving (Eq,Show,Enum)
-- @-node:gcross.20091208183517.1561:Coordinate
-- @+node:gcross.20091208183517.1583:Position
newtype Position a = Position [a] deriving (Eq,Show)
-- @-node:gcross.20091208183517.1583:Position
-- @-node:gcross.20091208183517.1560:Types
-- @+node:gcross.20091208183517.1459:Quantum Operators
-- @+node:gcross.20091208183517.1460:r_, x, y, z
r_ :: Num a => Coordinate -> DifferentialOperator a
r_ = multiplyByCoordinate
x = r_ X
y = r_ Y
z = r_ Z
-- @-node:gcross.20091208183517.1460:r_, x, y, z
-- @+node:gcross.20091208183517.1461:px, py, pz
p_ :: RealFloat a => Coordinate -> DifferentialOperator (Complex a)
p_ k = (-i) *| d k

px, py, pz :: RealFloat a => DifferentialOperator (Complex a)
px = p_ X
py = p_ Y
pz = p_ Z
-- @-node:gcross.20091208183517.1461:px, py, pz
-- @-node:gcross.20091208183517.1459:Quantum Operators
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

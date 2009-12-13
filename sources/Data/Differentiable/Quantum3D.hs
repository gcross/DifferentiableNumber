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
-- @+node:gcross.20091208183517.1460:r_
r_ :: Num a => Coordinate -> DifferentialOperator a
r_ = multiplyByCoordinate
-- @-node:gcross.20091208183517.1460:r_
-- @+node:gcross.20091208183517.1461:p_
p_ :: RealFloat a => Coordinate -> DifferentialOperator (Complex a)
p_ k = (-i) *| d k
-- @-node:gcross.20091208183517.1461:p_
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
-- @+node:gcross.20091212141130.1604:Functions
-- @+node:gcross.20091212141130.1605:v_
v_ :: Num a => Coordinate -> (a,a,a) -> FunctionExpansion a
v_ coordinate argument@(vx,vy,vz) = [vx,vy,vz] :-> 
    case coordinate of
        X -> vx ::> [one,zero,zero]
        Y -> vy ::> [zero,one,zero]
        Z -> vz ::> [zero,zero,one]
-- @-node:gcross.20091212141130.1605:v_
-- @-node:gcross.20091212141130.1604:Functions
-- @-others

-- @-node:gcross.20091208183517.1432:@thin Quantum3D.hs
-- @-leo

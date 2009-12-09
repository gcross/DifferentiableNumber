-- @+leo-ver=4-thin
-- @+node:gcross.20091208183517.1432:@thin Quantum3D.hs
-- @@language Haskell

module Data.Differential.Quantum3D where

-- @<< Import needed modules >>
-- @+node:gcross.20091208183517.1447:<< Import needed modules >>
import Data.Differentiable.FunctionExpansion
import Data.Differentiable.Number
-- @-node:gcross.20091208183517.1447:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091208183517.1560:Types
-- @+node:gcross.20091208183517.1561:Coordinate
data Coordinate = X | Y | Z
-- @-node:gcross.20091208183517.1561:Coordinate
-- @-node:gcross.20091208183517.1560:Types
-- @+node:gcross.20091208183517.1459:Operators
-- @+node:gcross.20091208183517.1460:x, y, z
x = multiplyByCoordinate 0
y = multiplyByCoordinate 1
z = multiplyByCoordinate 2
-- @-node:gcross.20091208183517.1460:x, y, z
-- @+node:gcross.20091208183517.1461:px, py, pz
px = d 0
py = d 1
pz = d 2
-- @-node:gcross.20091208183517.1461:px, py, pz
-- @-node:gcross.20091208183517.1459:Operators
-- @+node:gcross.20091208183517.1555:Instances
-- @+node:gcross.20091208183517.1557:Arbitrary
instance (Num a, Random a) => Arbitrary (DifferentiableNumber a) where
    arbitrary = mkStdGen $ \gen _ -> fst . randomR (zero,forFinitelyManyDerivatives 3 1) $ gen
-- @-node:gcross.20091208183517.1557:Arbitrary
-- @-node:gcross.20091208183517.1555:Instances
-- @-others
-- @-node:gcross.20091208183517.1432:@thin Quantum3D.hs
-- @-leo

-- @+leo-ver=4-thin
-- @+node:gcross.20091208183517.1406:@thin FunctionExpansion.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091208183517.1565:<< Language extensions >>
-- @-node:gcross.20091208183517.1565:<< Language extensions >>
-- @nl

module Data.Differentiable.FunctionExpansion where

-- @<< Import needed modules >>
-- @+node:gcross.20091208183517.1412:<< Import needed modules >>
import Control.Applicative
import Control.Exception

import Data.Differentiable
import Data.Differentiable.Number
-- @-node:gcross.20091208183517.1412:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091208183517.1407:Types
-- @+node:gcross.20091208183517.1408:FunctionExpansion
data FunctionExpansion a = [a] :-> DifferentiableNumber a
-- @-node:gcross.20091208183517.1408:FunctionExpansion
-- @+node:gcross.20091208183517.1413:DifferentialOperator
type DifferentialOperator a = FunctionExpansion a -> FunctionExpansion a
-- @-node:gcross.20091208183517.1413:DifferentialOperator
-- @+node:gcross.20091208183517.1445:DifferentialOperatorTransformation
type DifferentialOperatorTransformation a = DifferentialOperator a -> DifferentialOperator a -> DifferentialOperator a
-- @-node:gcross.20091208183517.1445:DifferentialOperatorTransformation
-- @-node:gcross.20091208183517.1407:Types
-- @+node:gcross.20091208183517.1429:Instances
-- @+node:gcross.20091208183517.1430:Differentiable
instance Differentiable (FunctionExpansion a) where
    differentiateBy index (argument :-> expansion) = argument :-> d index expansion
-- @-node:gcross.20091208183517.1430:Differentiable
-- @+node:gcross.20091208183517.1440:Show (Differentiable a)
instance Show a => Show (FunctionExpansion a) where
    show (argument :-> value) = show argument ++ " :-> " ++ show value
-- @-node:gcross.20091208183517.1440:Show (Differentiable a)
-- @+node:gcross.20091208183517.1449:Eq (Differentiable a)
instance Eq a => Eq (FunctionExpansion a) where
    (argument1 :-> value1) == (argument2 :-> value2) = argument1 == argument2 && value1 == value2
-- @-node:gcross.20091208183517.1449:Eq (Differentiable a)
-- @+node:gcross.20091208183517.1436:Num (Differentiable a)
instance Num a => Num (FunctionExpansion a) where
    (argument1 :-> expansion1) + (argument2 :-> expansion2) = assert (argument1 == argument2) $
        argument1 :-> (expansion1 + expansion2)
    (argument1 :-> expansion1) - (argument2 :-> expansion2) = assert (argument1 == argument2) $
        argument1 :-> (expansion1 - expansion2)
    (argument1 :-> expansion1) * (argument2 :-> expansion2) = assert (argument1 == argument2) $
        argument1 :-> (expansion1 * expansion2)
    fromInteger value = undefined :-> (constant . fromInteger $ value)
-- @-node:gcross.20091208183517.1436:Num (Differentiable a)
-- @-node:gcross.20091208183517.1429:Instances
-- @+node:gcross.20091208183517.1427:Differential Operators
-- @+node:gcross.20091208183517.1431:multiplyByCoordinate
multiplyByCoordinate :: (Num a, Enum i) => i -> DifferentialOperator a
multiplyByCoordinate i (argument :-> expansion) = argument :-> (variable index (argument !! index) * expansion)
  where index = fromEnum i
-- @-node:gcross.20091208183517.1431:multiplyByCoordinate
-- @-node:gcross.20091208183517.1427:Differential Operators
-- @+node:gcross.20091208183517.1442:Operators
-- @+node:gcross.20091208183517.1458:*|
infixl 7 *|
(*|) :: Num a => a -> DifferentialOperator a -> DifferentialOperator a
(*|) value operator function =
    let (arguments :-> expansion) = operator function
    in  (arguments :-> (constant value * expansion))
-- @-node:gcross.20091208183517.1458:*|
-- @+node:gcross.20091208183517.1444:~~
infixl 6 ~~
(~~) :: Num a => DifferentialOperatorTransformation a
a ~~ b = a.b |-| b.a
-- @-node:gcross.20091208183517.1444:~~
-- @+node:gcross.20091208183517.1456:|^
infixl 8 |^
(|^) :: DifferentialOperator a -> Int -> DifferentialOperator a
(|^) operator exponent
  | exponent < 0
    = error "Negative exponents not presently supported."
  | exponent == 0
    = id
  | exponent == 1
    = operator
  | otherwise
    = operator . operator |^(exponent-1)
-- @-node:gcross.20091208183517.1456:|^
-- @+node:gcross.20091208183517.1452:|-|/|+|
infixl 6 |+|, |-|
(|+|), (|-|) :: Num a => DifferentialOperatorTransformation a
(|+|) = liftA2 (+)
(|-|) = liftA2 (-)
-- @-node:gcross.20091208183517.1452:|-|/|+|
-- @+node:gcross.20091208183517.1454:osum/negosum
osum = foldl1 (|+|)
negosum = ((-1) *|) . osum
-- @-node:gcross.20091208183517.1454:osum/negosum
-- @-node:gcross.20091208183517.1442:Operators
-- @-others

-- @-node:gcross.20091208183517.1406:@thin FunctionExpansion.hs
-- @-leo

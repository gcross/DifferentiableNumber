-- @+leo-ver=4-thin
-- @+node:gcross.20091208183517.1406:@thin FunctionExpansion.hs
-- @@language Haskell

module Data.Differentiable.FunctionExpansion where

-- @<< Import needed modules >>
-- @+node:gcross.20091208183517.1412:<< Import needed modules >>
import Control.Applicative

import Data.Differentiable.Number
-- @-node:gcross.20091208183517.1412:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091208183517.1407:Types
-- @+node:gcross.20091208183517.1408:a :-> b
data (a :-> b) = ([a] :-> Differentiable b)
-- @-node:gcross.20091208183517.1408:a :-> b
-- @+node:gcross.20091208183517.1413:DifferentialOperator
type DifferentialOperator a b = (a :-> b) -> (a :-> b)
-- @-node:gcross.20091208183517.1413:DifferentialOperator
-- @+node:gcross.20091208183517.1445:DifferentialOperatorTransformation
type DifferentialOperatorTransformation a b = DifferentialOperator a b -> DifferentialOperator a b -> DifferentialOperator a b
-- @-node:gcross.20091208183517.1445:DifferentialOperatorTransformation
-- @-node:gcross.20091208183517.1407:Types
-- @+node:gcross.20091208183517.1429:Instances
-- @+node:gcross.20091208183517.1430:Differentiable
instance Differentiable b => Differentiable (a :-> b) where
    differentiateBy index (argument :-> expansion) = argument :-> d index expansion
-- @-node:gcross.20091208183517.1430:Differentiable
-- @+node:gcross.20091208183517.1440:Show (Differentiable i a)
instance (Show a, Show b) => Show (Differentiable a) where
    show (argument :-> value) = show argument ++ " :-> " ++ show value
-- @-node:gcross.20091208183517.1440:Show (Differentiable i a)
-- @+node:gcross.20091208183517.1449:Show (Differentiable i a)
instance (Eq a, Eq b) => Show (Differentiable a) where
    (argument1 :-> value1) == (argument2 :-> value2) = argument1 == argument2 && value1 == value2
-- @-node:gcross.20091208183517.1449:Show (Differentiable i a)
-- @+node:gcross.20091208183517.1436:Num (Differentiable i a)
instance Num b => Num (a -> b) where
    (argument1 :-> expansion1) + (argument2 :-> expansion2) = assert (argument1 == argument2) $
        argument1 :-> (expansion1 + expansion2)
    (argument1 :-> expansion1) - (argument2 :-> expansion2) = assert (argument1 == argument2) $
        argument1 :-> (expansion1 - expansion2)
    (argument1 :-> expansion1) * (argument2 :-> expansion2) = assert (argument1 == argument2) $
        argument1 :-> (expansion1 * expansion2)
    fromInteger value = 0 :-> (constant value)
-- @-node:gcross.20091208183517.1436:Num (Differentiable i a)
-- @-node:gcross.20091208183517.1429:Instances
-- @+node:gcross.20091208183517.1409:Functions
-- @+node:gcross.20091208183517.1410:linearInCoordinate
linearInCoordinate :: Int -> (a :-> a)
linearInCoordinate index argument = (argument !! index) ::> (replicate index zero ++ [one] ++ repeat zero)
-- @-node:gcross.20091208183517.1410:linearInCoordinate
-- @-node:gcross.20091208183517.1409:Functions
-- @+node:gcross.20091208183517.1427:Differential Operators
-- @+node:gcross.20091208183517.1431:multiplyByCoordinate
multiplyByCoordinate :: Int -> DifferentialOperator a a
multiplyByCoordinate index (argument :-> expansion) = argument :-> (argument !! index) * expansion
-- @-node:gcross.20091208183517.1431:multiplyByCoordinate
-- @-node:gcross.20091208183517.1427:Differential Operators
-- @+node:gcross.20091208183517.1442:Operators
-- @+node:gcross.20091208183517.1458:*|
infixl 7 *|
(*|) :: b -> DifferentialOperatorTransformation a b
(*|) value transformer f = ((Constant value) :*: (transformer f))
-- @-node:gcross.20091208183517.1458:*|
-- @+node:gcross.20091208183517.1444:~~
infixl 6 ~~
(~~) :: DifferentialOperatorTransformation a b
a ~~ b = a.b |-| b.a
-- @-node:gcross.20091208183517.1444:~~
-- @+node:gcross.20091208183517.1456:|^
infixl 8 |^
(|^) :: DifferentialOperator a b -> Int -> DifferentialOperator a b
(|^) operator exponent
  | exponent < 0
    = error "Negative exponents not presently supported."
  | exponent == 0
    = id
  | exponent == 1
    = transformer
  | otherwise
    = transformer . transformer|^(exponent-1)
-- @-node:gcross.20091208183517.1456:|^
-- @+node:gcross.20091208183517.1452:|-|/|+|
infixl 6 |+|, |-|
(|-|) = liftA2 (-)
(|+|) = liftA2 (+)
-- @-node:gcross.20091208183517.1452:|-|/|+|
-- @+node:gcross.20091208183517.1454:osum/negosum
osum = foldl1 (|+|)
negosum = ((-1) *|) . osum
-- @-node:gcross.20091208183517.1454:osum/negosum
-- @-node:gcross.20091208183517.1442:Operators
-- @-others
-- @-node:gcross.20091208183517.1406:@thin FunctionExpansion.hs
-- @-leo

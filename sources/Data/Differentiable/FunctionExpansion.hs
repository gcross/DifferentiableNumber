-- @+leo-ver=4-thin
-- @+node:gcross.20091208183517.1406:@thin FunctionExpansion.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091208183517.1565:<< Language extensions >>
{-# LANGUAGE TypeSynonymInstances #-}
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
-- @+node:gcross.20091208183517.1440:Show (FunctionExpansion a)
instance Show a => Show (FunctionExpansion a) where
    show (argument :-> value) = show argument ++ " :-> " ++ show value
-- @-node:gcross.20091208183517.1440:Show (FunctionExpansion a)
-- @+node:gcross.20091208183517.1449:Eq (FunctionExpansion a)
instance Eq a => Eq (FunctionExpansion a) where
    (_ :-> value1) == (_ :-> value2) = value1 == value2
-- @-node:gcross.20091208183517.1449:Eq (FunctionExpansion a)
-- @+node:gcross.20091212141130.1444:Ord (FunctionExpansion a)
instance Ord a => Ord (FunctionExpansion a) where
    compare (_ :-> value1) (_ :-> value2) = compare value1 value2
-- @-node:gcross.20091212141130.1444:Ord (FunctionExpansion a)
-- @+node:gcross.20091208183517.1436:Num (FunctionExpansion a)
instance Num a => Num (FunctionExpansion a) where
    (+) = binaryFunctionExpansionOperator (+)
    (-) = binaryFunctionExpansionOperator (-)
    (*) = binaryFunctionExpansionOperator (*)
    negate = unaryFunctionExpansionOperator negate
    fromInteger value = [] :-> (constant . fromInteger $ value)
-- @-node:gcross.20091208183517.1436:Num (FunctionExpansion a)
-- @+node:gcross.20091212141130.1438:Fractional (FunctionExpansion a)
instance Fractional a => Fractional (FunctionExpansion a) where
    (/) = binaryFunctionExpansionOperator (/)
    recip = unaryFunctionExpansionOperator recip
    fromRational value = [] :-> (constant . fromRational $ value)

-- @-node:gcross.20091212141130.1438:Fractional (FunctionExpansion a)
-- @+node:gcross.20091212141130.1440:Floating (FunctionExpansion a)
instance Floating a => Floating (FunctionExpansion a) where
    pi = [] :-> constant pi
    exp = unaryFunctionExpansionOperator exp
    sqrt = unaryFunctionExpansionOperator sqrt
    log = unaryFunctionExpansionOperator log
    (**) = binaryFunctionExpansionOperator (**)
    logBase = binaryFunctionExpansionOperator logBase
    sin = unaryFunctionExpansionOperator sin
    cos = unaryFunctionExpansionOperator cos
    tan = unaryFunctionExpansionOperator tan
    asin = unaryFunctionExpansionOperator asin
    acos = unaryFunctionExpansionOperator acos
    atan = unaryFunctionExpansionOperator atan
    sinh = unaryFunctionExpansionOperator sinh
    cosh = unaryFunctionExpansionOperator cosh
    asinh = unaryFunctionExpansionOperator asinh
    atanh = unaryFunctionExpansionOperator atanh
    acosh = unaryFunctionExpansionOperator acosh
-- @-node:gcross.20091212141130.1440:Floating (FunctionExpansion a)
-- @-node:gcross.20091208183517.1429:Instances
-- @+node:gcross.20091212141130.1433:Functions
-- @+node:gcross.20091212141130.1434:unaryFunctionExpansionOperator
unaryFunctionExpansionOperator ::
    (DifferentiableNumber a -> DifferentiableNumber a) ->
    FunctionExpansion a -> FunctionExpansion a
unaryFunctionExpansionOperator op (argument :-> value) = argument :-> op value
-- @-node:gcross.20091212141130.1434:unaryFunctionExpansionOperator
-- @+node:gcross.20091212141130.1436:binaryFunctionExpansionOperator
binaryFunctionExpansionOperator ::
    Eq a =>
    (DifferentiableNumber a -> DifferentiableNumber a -> DifferentiableNumber a) ->
    FunctionExpansion a -> FunctionExpansion a -> FunctionExpansion a
binaryFunctionExpansionOperator op (argument1 :-> value1) (argument2 :-> value2) =
    assert (argument1 == argument2) $
        argument1 :-> (value1 `op` value2)
-- @-node:gcross.20091212141130.1436:binaryFunctionExpansionOperator
-- @-node:gcross.20091212141130.1433:Functions
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
osum, negosum :: Num a => [DifferentialOperator a] -> DifferentialOperator a
osum = foldl1 (|+|)
negosum = ((-1) *|) . osum
-- @-node:gcross.20091208183517.1454:osum/negosum
-- @-node:gcross.20091208183517.1442:Operators
-- @-others

-- @-node:gcross.20091208183517.1406:@thin FunctionExpansion.hs
-- @-leo

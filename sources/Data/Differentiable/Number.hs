-- @+leo-ver=4-thin
-- @+node:gcross.20091208160243.1233:@thin Number.hs
-- @@language Haskell

module Data.Differentiable.Number where

-- @<< Import needed modules >>
-- @+node:gcross.20091208160243.1239:<< Import needed modules >>
import Control.Monad

import Data.Differentiable
import qualified Data.Stream as Stream

import Data.List
import Data.Tree

import System.Random

import Test.QuickCheck
-- @-node:gcross.20091208160243.1239:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091208183517.1423:Functions
-- @+node:gcross.20091208183517.1567:variable
variable :: (Enum i, Num a) => i -> a -> DifferentiableNumber a
variable i value = value ::> (replicate index zero ++ [one] ++ repeat zero)
  where index = fromEnum i

-- @-node:gcross.20091208183517.1567:variable
-- @+node:gcross.20091208183517.1465:forAllDerivatives
forAllDerivatives :: a -> DifferentiableNumber a
forAllDerivatives value = value ::> repeat (forAllDerivatives value)
-- @-node:gcross.20091208183517.1465:forAllDerivatives
-- @+node:gcross.20091208183517.1559:forFinitelyManyDerivatives
forFinitelyManyDerivatives :: Int -> a -> DifferentiableNumber a
forFinitelyManyDerivatives count value = value ::> replicate count (forFinitelyManyDerivatives count value)
-- @-node:gcross.20091208183517.1559:forFinitelyManyDerivatives
-- @+node:gcross.20091212141130.1420:invokeChainRule
invokeChainRule ::
    Num a =>
    (a -> a) ->
    (DifferentiableNumber a -> DifferentiableNumber a) ->
    DifferentiableNumber a -> DifferentiableNumber a
invokeChainRule function derivative argument@(value ::> derivatives) =
    function value ::> map (* derivative argument) derivatives
-- @-node:gcross.20091212141130.1420:invokeChainRule
-- @-node:gcross.20091208183517.1423:Functions
-- @+node:gcross.20091208160243.1234:Types
-- @+node:gcross.20091208160243.1236:DifferentiableNumber
data DifferentiableNumber a = a ::> [DifferentiableNumber a]
-- @-node:gcross.20091208160243.1236:DifferentiableNumber
-- @-node:gcross.20091208160243.1234:Types
-- @+node:gcross.20091208160243.1240:Instances
-- @+node:gcross.20091208183517.1425:Differentiable
instance Differentiable (DifferentiableNumber a) where
    differentiateBy i (_ ::> derivatives) = derivatives !! i
-- @-node:gcross.20091208183517.1425:Differentiable
-- @+node:gcross.20091208160243.1241:Show
instance Show a => Show (DifferentiableNumber a) where
    show (value ::> _) = show value
-- @-node:gcross.20091208160243.1241:Show
-- @+node:gcross.20091208160243.1242:Eq
instance Eq a => Eq (DifferentiableNumber a) where
    (value1 ::> _) == (value2 ::> _) = value1 == value2
-- @-node:gcross.20091208160243.1242:Eq
-- @+node:gcross.20091212141130.1446:Ord
instance Ord a => Ord (DifferentiableNumber a) where
    compare (value1 ::> _) (value2 ::> _) = compare value1 value2
-- @-node:gcross.20091212141130.1446:Ord
-- @+node:gcross.20091208160243.1243:Num
instance Num a => Num (DifferentiableNumber a) where
    (value1 ::> derivatives1) + (value2 ::> derivatives2) =
        (value1 + value2) ::> (zipWith (+) derivatives1 derivatives2)
    (value1 ::> derivatives1) - (value2 ::> derivatives2) =
        (value1 - value2) ::> (zipWith (-) derivatives1 derivatives2)
    number1@(value1 ::> derivatives1) * number2@(value2 ::> derivatives2) =
        (value1 * value2) ::> zipWith (+) (map (number1*) derivatives2)
                                          (map (number2*) derivatives1)
    negate (value ::> derivatives) =
        negate value ::> map negate derivatives

    fromInteger = constant . fromInteger
-- @-node:gcross.20091208160243.1243:Num
-- @+node:gcross.20091212141130.1418:Fractional
instance Fractional a => Fractional (DifferentiableNumber a) where
    recip = invokeChainRule recip (\x -> -(recip x)*(recip x))

    fromRational = constant . fromRational
-- @-node:gcross.20091212141130.1418:Fractional
-- @+node:gcross.20091212141130.1419:Floating
instance Floating a => Floating (DifferentiableNumber a) where
    pi = constant pi

    exp = invokeChainRule exp exp
    sqrt = invokeChainRule sqrt (\x -> constant 0.5 * recip (sqrt x))
    log = invokeChainRule log recip
    sin = invokeChainRule sin cos
    cos = invokeChainRule cos (negate . sin)
    tan = invokeChainRule tan (\x -> recip (cos x * cos x))
    asin = invokeChainRule asin (\x -> (recip . sqrt) (one - x*x))
    acos = invokeChainRule acos (\x -> (negate . recip . sqrt) (one - x*x))
    atan = invokeChainRule atan (\x -> recip (one + x*x))
    sinh = invokeChainRule sinh cosh
    cosh = invokeChainRule cosh sinh
    tanh = invokeChainRule tanh (\x -> one - (tanh x)*(tanh x))
    asinh = invokeChainRule asin (\x -> (recip . sqrt) (x*x + 1))
    acosh = invokeChainRule acos (\x -> (recip . sqrt) (x*x - 1))
    atanh = invokeChainRule atan (\x -> recip (one - x*x))
-- @-node:gcross.20091212141130.1419:Floating
-- @+node:gcross.20091212141130.1652:HasConstants
instance HasConstants DifferentiableNumber where
    constant value = value ::> repeat zero
-- @-node:gcross.20091212141130.1652:HasConstants
-- @-node:gcross.20091208160243.1240:Instances
-- @+node:gcross.20091209020922.1337:Generators
-- @+node:gcross.20091209020922.1339:Arbitrary (Tree a)
instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = liftM2 Node arbitrary (fmap Stream.toList arbitrary)
-- @-node:gcross.20091209020922.1339:Arbitrary (Tree a)
-- @+node:gcross.20091209020922.1341:Arbitrary (DifferentiableNumber a)
instance Arbitrary a => Arbitrary (DifferentiableNumber a) where
    arbitrary = fmap (extractDerivatives []) arbitrary
      where
        extractDerivatives :: [Int] -> Tree a -> DifferentiableNumber a
        extractDerivatives branchings tree =
            go branchings tree ::> map (flip extractDerivatives tree . flip insert branchings) [0..]

        go [] node = rootLabel node
        go (next:rest) node = go rest ((subForest node) !! next)
-- @-node:gcross.20091209020922.1341:Arbitrary (DifferentiableNumber a)
-- @-node:gcross.20091209020922.1337:Generators
-- @-others
-- @-node:gcross.20091208160243.1233:@thin Number.hs
-- @-leo

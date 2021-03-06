-- @+leo-ver=4-thin
-- @+node:gcross.20091209122152.1337:@thin Testing.hs
-- @@language Haskell

module Data.Differentiable.Testing where

-- @<< Import needed modules >>
-- @+node:gcross.20091209122152.1456:<< Import needed modules >>
import Control.Applicative
import Control.Monad

import Data.Complex
import Data.List
import Data.Ratio

import Debug.Trace

import System.Random

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Data.Differentiable
import Data.Differentiable.FunctionExpansion
import Data.Differentiable.Number
-- @-node:gcross.20091209122152.1456:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091220080702.2365:Types
-- @+node:gcross.20091220080702.2366:Argument
newtype Argument a = Argument { getArg :: [a] }

-- @-node:gcross.20091220080702.2366:Argument
-- @+node:gcross.20091220080702.2399:ForceTypeToBe
type ForceTypeToBe a = a -> a
-- @-node:gcross.20091220080702.2399:ForceTypeToBe
-- @-node:gcross.20091220080702.2365:Types
-- @+node:gcross.20091209122152.1450:Operators
-- @+node:gcross.20091209122152.1452:===
infixl 4 ===
(===) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
(===) = liftA2 (==)
-- @-node:gcross.20091209122152.1452:===
-- @+node:gcross.20091209122152.1454:=~=
infixl 4 =~=
(=~=) :: AlmostEq b => (a -> b) -> (a -> b) -> a -> Bool
(=~=) = liftA2 (~=)
-- @-node:gcross.20091209122152.1454:=~=
-- @-node:gcross.20091209122152.1450:Operators
-- @+node:gcross.20091209122152.1448:Classes
-- @+node:gcross.20091209122152.1449:AlmostEq
infix 4 ~=, /~
class AlmostEq a where
    (~=) :: a -> a -> Bool

instance AlmostEq Double where
    x ~= y = min (abs (x-y)) (abs (x-y) / (abs (x+y) + 1e-100)) < 1e-7

instance AlmostEq a => AlmostEq [a] where
    x ~= y = all (uncurry (~=)) $ zip x y

instance (AlmostEq a, RealFloat a) => AlmostEq (Complex a) where
    (a :+ b) ~= (c :+ d) = (a ~= c) && (b ~= d)

instance AlmostEq a => AlmostEq (DifferentiableNumber a) where
    (x ::> _) ~= (y ::> _) = x ~= y

instance AlmostEq a => AlmostEq (FunctionExpansion a) where
    (_ :-> x) ~= (_ :-> y) = x ~= y

x /~ y = not (x ~= y)
-- @-node:gcross.20091209122152.1449:AlmostEq
-- @-node:gcross.20091209122152.1448:Classes
-- @+node:gcross.20091209122152.1462:Instances
-- @+node:gcross.20091220080702.2371:Show (Argument a)
instance Show a => Show (Argument a) where
    show argument = "[" ++ intercalate "," (map show . take 3 . getArg $ argument) ++ ",...]"
-- @-node:gcross.20091220080702.2371:Show (Argument a)
-- @+node:gcross.20091209122152.1464:Random (Complex a)
instance (Random a, RealFloat a) => Random (Complex a) where
    randomR (lo_r :+ lo_i,hi_r :+ hi_i) g0 =
        let (r,g1) = randomR (lo_r,hi_r) g0
            (i,g2) = randomR (lo_i,hi_i) g1
        in (r :+ i,g2)
    random g0 =
        let (r,g1) = random g0
            (i,g2) = random g1
        in (r :+ i,g2)
-- @-node:gcross.20091209122152.1464:Random (Complex a)
-- @+node:gcross.20091212141130.1442:Random (Rational a)
instance (Integral a, Random a) => Random (Ratio a) where
    randomR (lo,hi) g0 =
        let (n,g1) = randomR (numerator lo,numerator hi) g0
            (d,g2) = randomR (denominator lo,denominator hi) g1
        in (n % d,g2)
    random g0 =
        let (n,g1) = random g0
            (d,g2) = random g1
        in (n % d,g2)
-- @-node:gcross.20091212141130.1442:Random (Rational a)
-- @-node:gcross.20091209122152.1462:Instances
-- @+node:gcross.20091209122152.1459:Generators
-- @+node:gcross.20091209122152.1461:Complex Double
instance (Arbitrary a, RealFloat a) => Arbitrary (Complex a) where
    arbitrary = liftM (:+ 0) $ (resize 1) arbitrary
-- @-node:gcross.20091209122152.1461:Complex Double
-- @+node:gcross.20091220080702.2368:Argument
instance Random a => Arbitrary (Argument a) where
    arbitrary = MkGen $ \stdgen size -> Argument (randoms stdgen)
-- @-node:gcross.20091220080702.2368:Argument
-- @+node:gcross.20091220080702.2370:FunctionExpansion
instance (Num a, Random a, Arbitrary a) => Arbitrary (FunctionExpansion a) where
    arbitrary = MkGen $ \gen size ->
        let (gen1,gen2) = split gen
            (Argument argument) = unGen arbitrary gen1 size
            expansion = unGen arbitrary gen2 size
        in argument :-> expansion
-- @-node:gcross.20091220080702.2370:FunctionExpansion
-- @-node:gcross.20091209122152.1459:Generators
-- @+node:gcross.20091209122152.1468:Functions
-- @+node:gcross.20091209122152.1469:echo
echo x = trace (show x) x
-- @-node:gcross.20091209122152.1469:echo
-- @-node:gcross.20091209122152.1468:Functions
-- @-others

-- @-node:gcross.20091209122152.1337:@thin Testing.hs
-- @-leo

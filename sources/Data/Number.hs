-- @+leo-ver=4-thin
-- @+node:gcross.20091208160243.1233:@thin Number.hs
-- @@language Haskell

module Data.Differential.Number where

-- @<< Import needed modules >>
-- @+node:gcross.20091208160243.1239:<< Import needed modules >>
-- @-node:gcross.20091208160243.1239:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091208183517.1421:Values
-- @+node:gcross.20091208183517.1422:zero
zero = 0 ::> repeat zero
-- @-node:gcross.20091208183517.1422:zero
-- @-node:gcross.20091208183517.1421:Values
-- @+node:gcross.20091208183517.1423:Functions
-- @+node:gcross.20091208183517.1426:constant
constant :: a -> DifferentiableNumber a
constant value = value ::> repeat zero
-- @-node:gcross.20091208183517.1426:constant
-- @+node:gcross.20091208183517.1465:forAllDerivatives
forAllDerivatives :: a -> DifferentiableNumber a
forAllDerivatives value = value ::> repeat (forAllDerivatives value)
-- @-node:gcross.20091208183517.1465:forAllDerivatives
-- @+node:gcross.20091208183517.1559:forFinitelyManyDerivatives
forFinitelyManyDerivatives :: Int -> a -> DifferentiableNumber a
forFinitelyManyDerivatives count value = value ::> replicate count (forFinitelyManyDerivatives value)
-- @-node:gcross.20091208183517.1559:forFinitelyManyDerivatives
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
instance Show (DifferentiableNumber a) where
    show (value ::> _) = value
-- @-node:gcross.20091208160243.1241:Show
-- @+node:gcross.20091208160243.1242:Eq
instance Eq (DifferentiableNumber a) where
    show (value1 ::> _) (value2 ::> _) = value1 == value2
-- @-node:gcross.20091208160243.1242:Eq
-- @+node:gcross.20091208160243.1243:Num
instance Num a => Num (DifferentiableNumber a) where
    (value1 ::> derivatives1) + (value2 ::> derivatives2) =
        (value1 + value2) :-> (zipWith (+) derivatives1 derivatives2)
    (value1 ::> derivatives1) - (value2 ::> derivatives2) =
        (value1 - value2) ::> (zipWith (-) derivatives1 derivatives2)
    number1@(value1 ::> derivatives1) * number2@(value2 ::> derivatives2) =
        (value1 * value2) ::> zipWith (+) (map (number1*) derivatives1)
                                          (map (number2*) derivatives2)
    fromInteger = constant
-- @-node:gcross.20091208160243.1243:Num
-- @+node:gcross.20091208183517.1464:Random
instance Random a => Random (DifferentiableNumber a) where
    randomR (lo ::> lodiffs,hi ::> hidiffs) gen =
        let (gen1,gen2) = split gen
            (value,returned_gen) = randomR (lo,hi) gen1
            diffs = go lodiffs hidiffs gen2
              where
                go [] _ _ = []
                go _ [] _ = []
                go (lodiff:rest_lodiffs) (hidiff:rest_hidiffs) old_gen =
                    let (diff,new_gen) = randomR (lodiff,hidiff) old_gen
                    in diff:go rest_lodiffs rest_hidiffs new_gen
        in (value ::> diffs,returned_gen)
    random = randomR (zero,forAllDerivatives 1)
-- @-node:gcross.20091208183517.1464:Random
-- @-node:gcross.20091208160243.1240:Instances
-- @-others
-- @-node:gcross.20091208160243.1233:@thin Number.hs
-- @-leo

-- @+leo-ver=4-thin
-- @+node:gcross.20091208183517.1416:@thin Differentiable.hs
-- @@language Haskell

module Data.Differentiable where


-- @+others
-- @+node:gcross.20091208183517.1419:Classes
-- @+node:gcross.20091208183517.1420:Differentiable
class Differentiable a where
    differentiateBy :: Int -> a -> a
    d :: Enum i => i -> a -> a
    d i = differentiateBy (fromEnum i)
-- @-node:gcross.20091208183517.1420:Differentiable
-- @+node:gcross.20091212141130.1651:HasConstants
class HasConstants a where
    constant :: Num n => n -> a n
    zero :: Num n => a n
    one :: Num n => a n

    zero = constant 0
    one = constant 1

-- @-node:gcross.20091212141130.1651:HasConstants
-- @-node:gcross.20091208183517.1419:Classes
-- @-others
-- @-node:gcross.20091208183517.1416:@thin Differentiable.hs
-- @-leo

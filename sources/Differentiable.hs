-- @+leo-ver=4-thin
-- @+node:gcross.20091208183517.1416:@thin Differentiable.hs
-- @@language Haskell

module Data.Differentiable where


-- @+others
-- @+node:gcross.20091208183517.1419:Classes
-- @+node:gcross.20091208183517.1420:Differentiable
class Differentiable a where
    differentiateBy :: Int -> a -> a
    d :: Enum -> a -> a
    d i = differentiateBy (fromEnum i)
-- @-node:gcross.20091208183517.1420:Differentiable
-- @-node:gcross.20091208183517.1419:Classes
-- @-others
-- @-node:gcross.20091208183517.1416:@thin Differentiable.hs
-- @-leo

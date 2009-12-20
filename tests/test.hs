-- @+leo-ver=4-thin
-- @+node:gcross.20091220080702.1868:@thin test.hs
-- @@language Haskell

-- @<< Import needed modules >>
-- @+node:gcross.20091220080702.1870:<< Import needed modules >>
import Data.Complex

import Test.Framework

import qualified MultiParticle as MP
import qualified SingleParticle as SP
-- @-node:gcross.20091220080702.1870:<< Import needed modules >>
-- @nl

-- @+others
-- @-others

main = defaultMain
    [testGroup "Data.Differentiable.Number.Quantum" SP.tests
    ,testGroup "Data.Differentiable.Number.Quantum.MultiParticle" MP.tests
    ]
-- @-node:gcross.20091220080702.1868:@thin test.hs
-- @-leo

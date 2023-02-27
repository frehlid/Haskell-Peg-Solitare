-- CPSC 312 - 2023 - Games in Haskell
module Hash where

class Hash t where
   hash :: t -> Int

arbMun = 0.5 * (sqrt(5) -1)  :: Double
numHashVals = 2^20  :: Double
fractionalPart :: Double -> Double
fractionalPart n = n- fromIntegral(floor n)

instance Hash Int where
   hash n = floor (numHashVals *fractionalPart(arbMun *fromIntegral n))
   -- this is based on the multiplication method described by Cormen at al (1990) Introduction to Algorithms

instance Hash t => Hash [t] where
   hash [] = 1741        -- any constant
   hash (h:t) = hash ( hash h + hash t)

-- ? Why not define hash for lists as:
hashsum lst =  (sum [hash e | e <- lst])

-- For useful code you probably want to use the standard Hashable class
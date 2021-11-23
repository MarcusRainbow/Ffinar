module Utils (foldlr) where

-- |The problem with foldl and scanl is that they are strict in the
-- |input list. The problem with foldr and scanr is that the function
-- |cannot access preceding elements, which prevents running sums or NPVs.
-- |
-- |foldlr takes a function and an input list, together with two
-- |initialisers, one for the left-hand accumulator and one for the right.
-- |
-- |Within the function, the parameters are the value being merged, the 
-- |left-hand accumulator and the right-hand accumulator.
foldlr :: (a -> b -> [b] -> [b]) -> b -> [b] -> [a] -> [b]
foldlr f l r xs =
    let result = foldr (\(l', x) r' -> f x l' r') r (zip (l:result) xs) in
        result

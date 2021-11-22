module Utils (foldlr) where

foldlr :: (a -> b -> [b] -> [b]) -> b -> [b] -> [a] -> [b]
foldlr f l r xs =
    let result = foldr (\(l', x) r' -> f x l' r') r (zip (l:result) xs) in
        result

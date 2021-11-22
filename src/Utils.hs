module Utils (foldlr) where

running_sum :: Num a => [a] -> [a]
running_sum xs = foldlr (\x l r -> (x + l):r) 0 [] xs

foldlr :: (a -> b -> [b] -> [b]) -> b -> [b] -> [a] -> [b]
foldlr f l r xs =
    let result = foldr (\(l', x) r' -> f x l' r') r (zip (l:result) xs) in
        result

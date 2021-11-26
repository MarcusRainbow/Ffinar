module Utils (
    foldlr,
    revcat,
    buffer,
    approxSort,
    lazySort,
    sortedMap) where

import Deque.Lazy as Q
import Data.Foldable as F
-- import Debug.Trace

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

-- |
-- Given a function that maps a sorted list of items, apply that function
-- to a roughly sorted list, to give a list of
-- results that match order of the original list. The meaning of rough is
-- defined by a function that is passed in that says whether two items
-- differ by too much to count as roughly the same.
sortedMap :: Ord a => (a -> a -> Bool) -> ([a] -> [b]) -> [a] -> [b]
sortedMap f m xs = 
    let
        -- generate a sorted list of indexed items, then extract a list of items
        xsi = lazySortBy comp rough (zip xs [0..]) where
            comp = (\(x,_) (y,_) -> compare x y) 
            rough = (\(x,_) (y,_) -> f x y) 
        xs' = map (\(x,i) -> x) xsi

        -- apply the function to the now sorted list
        ys = m xs'

        -- now reapply the indices and sort into index order
        ysi = lazySortBy comp rough (zip ys xsi) where
            comp = (\(_,(_,i)) (_,(_,j)) -> compare i j)
            rough = (\(_,(x,_)) (_,(y,_)) -> f x y)
    in
        -- finally remove the indices (and x values)
        map (\(y,_) -> y) ysi

-- sortedMap :: ([a] -> [b]) -> [a] -> [b]
-- sortedMap f a = unsort i b where
--     (a', i) = indexSort a
--     b = f a'

-- indexSort :: (Ord a) => [a] -> ([a], [Int])

-- sortedMerge :: [a] -> [a] -> ([a], [Int])

-- unsort :: [Int] -> [b] -> [b]

-- -- |Merges two roughly sorted lists to produce another
-- -- |that is exactly sorted. Duplicates are kept.
-- approxMerge :: (Ord a) => [a] -> [a] -> [a]
-- approxMerge a b = approxMerge' a b []

-- -- |Same as approxMerge, but also takes a third parameter
-- -- |which is the elements already output.
-- approxMerge' :: (Ord a) => [a] -> [a] -> [a] -> [a]
-- approxMerge a [] _ = sort a  -- maybe worth making nonstrict
-- approxMerge [] b = sort b  -- ditto
-- approxMerge as@(a:as') bs@(b:bs') z = case a `compare` b of
--     LT -> a : approxMerge as' bs
--     EQ -> a : b : approxMerge as' bs'
--     GT -> b : approxMerge as bs'

-- -- |Merges two roughly sorted lists to produce another
-- -- |that is exactly sorted. Duplicates are kept.
-- approxMergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
-- approxMergeBy f a [] = sortBy f a
-- approxMergeBy f [] b = sortBy f b
-- approxMergeBy f as@(a:as') bs@(b:bs') = case f a b of
--     LT -> a : approxMergeBy f as' bs
--     EQ -> a : b : approxMergeBy f as' bs'
--     GT -> b : approxMergeBy f as bs'

-- |
-- Version of lazy sort that never flushes
approxSort :: (Ord a) => [a] -> [a]
approxSort = lazySort (\_ _ -> False)

-- |
-- Non-strictly sorts a list that is already approximately correct. First
-- argument is a flush function: flush l r. If r - l is large enough, then
-- flush the output.
lazySort :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
lazySort = lazySortBy compare

-- |
-- Non-strictly sorts a list that is already approximately correct, given a
-- sorting function.
lazySortBy :: (a -> a -> Ordering) -> (a -> a -> Bool) -> [a] -> [a]
lazySortBy c f a = lazySortBy' c f a sdEmpty

-- |lazy sorts list into an intermediate sorted deque and then into a list
lazySortBy' :: (a -> a -> Ordering) -> (a -> a -> Bool) -> [a] -> SortedDeque a -> [a]
lazySortBy' _ _ [] d = sdToList d
lazySortBy' c f (x:xs) d = let (d', o) = sdFlush f x d in
    revcat o $ lazySortBy' c f xs (sdInsert c x d')

-- |
-- A container that supports rapid sorted insertion so long
-- as the elements are roughly supported anyway, and also
-- supports removal of elements from the back. It comprises
-- a forward-order list followed by a reverse-order deque.
-- It also holds an output list, which is written to from time
-- to time.
--
-- The idea is that normal order items are prepended to the
-- reverse-order deque, and they are removed from the back of
-- that deque in correct order. Incoming out-of-order items
-- cause items to be taken off the deque and put on the
-- forward-order holding list until the new item can be inserted
-- at the head of the deque.
data SortedDeque a = SD ([a], Q.Deque a) deriving Show

-- |An empty sorted deque
sdEmpty :: SortedDeque a
sdEmpty = SD ([], Q.fromConsAndSnocLists [] [])

-- |
-- Insert an item into a sorted deque. The insertion point between
-- backward list and forward deque is shuffled until the new item
-- inserts at the head of the deque. 
sdInsert :: (a -> a -> Ordering) -> a -> SortedDeque a -> SortedDeque a
sdInsert c a d@(SD ([], _)) = sdForwardInsert c a d
sdInsert c a d@(SD (xs@(x:xs'), q)) = 
    -- trace ("sdInsert " ++ show (a, d)) $ 
    case c a x of
        LT -> sdForwardInsert c a d    -- insert in deq somewhere
        EQ -> sdPrepend a d            -- insert at start of deq (breaks stable sort)
        GT -> sdInsert c a (SD (xs', Q.cons x q))  -- shuffle and try again

-- |
-- Here we know that the element to be inserted is greater than anything
-- in the back list. Insert it somewhere in the forward deque.
sdForwardInsert :: (a -> a -> Ordering) -> a -> SortedDeque a -> SortedDeque a
sdForwardInsert c a d@(SD (xs, q)) = 
    -- trace ("sdForwardInsert " ++ show (a, d)) $ 
    case Q.head q of
        Nothing -> sdPrepend a d    -- start a new deque with the element at the start
        Just y  -> case c a y of
            LT  -> sdForwardInsert c a (SD (y:xs, Q.tail q))
            EQ  -> sdPrepend a d    -- insert the element at the start of the deque
            GT  -> sdPrepend a d    -- ditto

-- |
-- Prepends an item to the start of the sorted deque
sdPrepend :: a -> SortedDeque a -> SortedDeque a
sdPrepend a (SD (xs, q)) = SD (xs, Q.cons a q)

-- |
-- if appropriate shifts some items onto the output so there 
-- is not too much in the deque
sdFlush :: (a -> a -> Bool) -> a -> SortedDeque a -> (SortedDeque a, [a])
sdFlush f a (SD (xs, q)) = 
    let 
        rev = Q.reverse q              -- reverse so we can look at the back - O(1)
        (out, keep) = Q.span (f a) rev
        keep' = Q.reverse keep         -- reverse what's left in the deque
        o' = F.toList (Q.reverse out)  -- stuff to put on the output
    in
        -- trace ("sdFlush " ++ show (a, (xs, q), keep', o')) $
        (SD (xs, keep'), o')

-- |
-- Gets a sorted deque to spill out all its contents as a list
sdToList :: SortedDeque a -> [a]
sdToList d@(SD (x, q)) =
    -- trace ("sdToList " ++ show d) $
    revcat (F.toList q) x

-- |
-- Slightly quicker than reverse ++
revcat :: [a] -> [a] -> [a]
revcat [] ys = ys
revcat (x:xs) ys = revcat xs (x:ys)

-- |
-- Test non-strict evaluation. Copies a list but buffering
-- it into strict chunks according to some function of the
-- values.
buffer :: (a -> Bool) -> [a] -> [a]
buffer f a = buffer' f a []

-- |Used within the implementation of nonStrict
buffer' :: (a -> Bool) -> [a] -> [a] -> [a]
buffer' _ [] b = Prelude.reverse b  -- if input is empty, output the buffer
buffer' f (a:as) b
    | f a       = revcat b (buffer' f as [])
    | otherwise = buffer' f as (a:b)

module MergeSort (sortBy, sort) where

halves :: [a] -> ([a], [a])
halves = flip splitAt <*> (`div` 2) . length

align :: Ord a => (a -> a -> Bool) -> ([a], [a]) -> [a]
align predicate elements =

    case elements of
    (xs, []) -> xs
    ([], ys) -> ys
    (lx@(x:xs), ly@(y:ys)) | x `predicate` y -> x : recurse (xs, ly)
                           |    otherwise    -> y : recurse (lx, ys)
    where
        recurse = align predicate

merge :: Ord a => (a -> a -> Bool) -> ([a], [a]) -> [a]
merge predicate elements =

    case elements of
    ([x], [y]) -> predicate `align` (        [x],         [y])
    ([x], lys) -> predicate `align` (        [x], recurse lys)
    (lxs, [y]) -> predicate `align` (recurse lxs,         [y])
    (lxs, lys) -> predicate `align` (recurse lxs, recurse lys)
    where
        recurse = merge predicate . halves
        
sortBy :: Ord a => (a -> a -> Bool) -> [a] -> [a]
sortBy predicate = merge predicate . halves

sort :: Ord a => [a] -> [a]
sort = sortBy (<=)

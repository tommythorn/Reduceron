module MergeSort(group,unique) where

import List(sort)

group :: (Ord t) => [t] -> [[t]]
group l = groupSorted (sort l)

groupSorted :: (Eq t) => [t] -> [[t]]
groupSorted [] = []
groupSorted (x0:xs) = groupSorted' x0 [] xs
        where
            groupSorted' x a [] = [x:a]
            groupSorted' x a (y:ys) =
                if x == y
                then groupSorted' x (y:a) ys
                else (x:a) : groupSorted' y [] ys

unique :: (Ord a) => [a] -> [a]
unique xs = map head (group xs)

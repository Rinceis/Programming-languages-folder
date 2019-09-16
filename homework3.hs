module A3 where
-- | removeAllExcept takes in a value and a list of values, goes through the list to see if there are any that don't match the first value, and if there are, skips over them when producing the new list of values

removeAllExcept :: Eq a => a -> [a] -> [a]
removeAllExcept _ [] = []
removeAllExcept x (z:zs)
  | x == z = z: removeAllExcept x zs
  | otherwise =  removeAllExcept x zs

  -- | removeAll takes in a value and a list of values, goes through the list to see if there are any that match the first value, and if there are, skips over them when producing the new list of values

removeAll :: Eq a => a -> [a] -> [a]
removeAll _ [] = []
removeAll x (z:zs)
    | x == z = removeAll x zs
    | otherwise = z : removeAll x zs

    -- | substitute takes in two values and a list of values, and goes through the list to see if there are any values present that match the first value. If there are, it replaces them with the second value when producing the new list of values
substitute :: Eq a => a -> a -> [a] -> [a]
substitute _ _ [] = []
substitute x y (z:zs)
    | x == z = y : substitute x y zs
    | otherwise = z : substitute x y zs

    -- | mergeSorted3 takes in three sorted lists of values, and goes through the lists to find the combination that makes it so that the list goes from lowest to highest when all the values are put together in one list. the new list produced is as a result sorted from least to most.

mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
mergeSorted3 [][][] = []
mergeSorted3 (x:xs) (y:ys) (z:zs)
    | x <= y && x <= z = x : mergeSorted3 xs (y:ys) (z:zs)
    | y <= x && y <= z = y : mergeSorted3 (x:xs) ys (z:zs)
    | z <= x && z <= y = z : mergeSorted3 (x:xs) (y:ys) zs
mergeSorted3 (x:xs) (y:ys) []
    | y <= x = y : mergeSorted3 (x:xs) ys []
    | x <= y = x : mergeSorted3 xs (y:ys) []
mergeSorted3 (x:xs) [] (y:ys)
    | y <= x = y : mergeSorted3 (x:xs) ys []
    | x <= y = x : mergeSorted3 xs (y:ys) []
mergeSorted3 [] (x:xs) (y:ys)
    | y <= x = y : mergeSorted3 (x:xs) ys []
    | x <= y = x : mergeSorted3 xs (y:ys) []

mergeSorted3 (x:xs) [] []
    | x == x = x : mergeSorted3 xs [] []
    | x /= x = x : mergeSorted3 [] [] []

mergeSorted3 [] (x:xs) []
    | x == x = x : mergeSorted3 [] xs []
    | x /= x = x : mergeSorted3 [] [] []


mergeSorted3 [] [] (x:xs)
    | x == x = x : mergeSorted3 [] [] xs
    | x /= x = x : mergeSorted3 [] [] []



-- | please tell me if you can get this figured out, as apparently there's a parse error, even when there shouldn't be one. The code itself works, as far as I can tell.

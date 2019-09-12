module A2 where
-- | find minimum element in a list of numbers
-- | consumes a list of numbers
-- | produces the single smallest number on the list

findMin theList = if theList ==[]
                  then 1/0
                  else if (head theList) <= (findMin(tail theList))
                    then (head theList)
                    else (findMin(tail theList))


-- | return the dot product of two lists
-- | consumes two lists of numbers
-- | produces a iteratorue based on the formula dot(q,p) = (q1*p1)+(q2*p2)+...+(qn*pn)

tupleDotProduct (a,b) = if a ==[] || b ==[]
                        then 0
                        else ((head a) * (head b)) + tupleDotProduct ((tail a), (tail b))


-- | from two lists, return a list with a pair from each list consecutively, but in reverse
-- | consumes two lists
-- | produces a list where the final element in each list is paired together
-- with the latter list having their elements as the first part of each entry
-- in the new list, then the second-to-last... etc.

revZip2Lists (a, b) = if a ==[] || b ==[]
                      then []
                      else (revZip2Lists(tail a, tail b)) ++[(head b, head a)]

-- | construct a new list consisting only of every third element of the list put in
-- | consumes a list of elements
-- | produces a list of every third element from the input.

everyThird :: [val] -> [val]
everyThird as = iterator 0 as
  where iterator _ [] =[]
        iterator 0 (a : as) = a : iterator 2 as
        iterator n (_ : as) = iterator(n - 1) as

--based on my understanding from here: https://stackoverflow.com/a/48554565

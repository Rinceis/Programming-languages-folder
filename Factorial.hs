--  Double elements in a list of numbers
-- Consumes a list of numbers
-- produces a list of numbers where each element is multiplied by 2

doubleList theList = if theList == []
                        then []
                        else 2* (head theList) : doubleList (tail theList)

reverseList theList = if theList == []
                      then []
                      (reverseList (tail theList)) ++ [(head theList)]
-- lenghs of Strings
-- consume a list of words
-- produce a list of their lengths
lenghts theList = if theList == []
                  then []
                  



factorial n = if n == 0
                  then 1
                  else n*factorial (n-1)

newFact 0 = 1
newFact n = n*newFact (n-1)

multOne a b = a * b
multTwo (a, b) = a * b

mymax (a,b) = if a < b then b else a


dotproduct (a,b) (c,d) = a+c + b+d

byTwo (a, b) = (2*a, 2*b)

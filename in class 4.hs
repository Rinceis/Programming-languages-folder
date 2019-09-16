data MyType = TwoInts Int Int | Spaghetti [Char] | Pizza
myFun x = case x of
  TwoInts xy -> x + y
  Spaghetti name -> 0
  Pizza -> 100

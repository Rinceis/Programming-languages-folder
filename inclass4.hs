data MyType = TwoInts Int Int | Spaghetti [Char] | Pizza
  deriving (Show)

myFun x = case x of
  TwoInts x y -> x + y
  Spaghetti name -> 0
  Pizza -> 100

data MyList a = Empty | Cons a (MyList a)
      deriving (Show)

myAdd theList = case theList of
  Empty -> 0
  Cons u tailList -> u + (myAdd tailList)


modify f theList = case theList of
                  Empty -> Empty
                  Cons u tailList -> Cons (f u) (modify f tailList)


data MyTree a = EmptyTree | Node a (MyTree a) (MyTree a) (MyTree a)
  deriving (Show)

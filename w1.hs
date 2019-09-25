module W1 where

data TriTree a = EmptyNode  | TriNode a (TriTree a)(TriTree a)(TriTree a)
    deriving (Show)

instance (Eq a) => Eq (TriTree a) where
  EmptyNode           == EmptyNode  = True
  TriNode a la ma ra  == TriNode b lb mb rb = (a == b) &&
                                              (la == lb) &&
                                              (ma == mb) &&
                                              (ra == rb)
  _                   == _ = False


-- | nodeValue is a function which takes in a trinary tree and returns
-- | the value (a) for the given node, returning an error if it's an emptyNode
nodeValue :: TriTree a -> a
nodeValue EmptyNode = error "no empty nodes should be submitted"
nodeValue (TriNode a x y z) = a

-- | leftChild is a function which takes in a trinary tree and returns
-- | the value of the left child, here x, for the given node tree.
-- | It returns an error if input is an emptyNode
leftChild :: TriTree a -> TriTree a
leftChild EmptyNode = error "no empty nodes should be submitted"
leftChild (TriNode a x y z) = x

-- | middleChild is a function which takes in a trinary tree and returns
-- | the value of the middle child, here y, for the given node tree.
-- | It returns an error if input is an emptyNode
middleChild :: TriTree a -> TriTree a
middleChild EmptyNode = error "no empty nodes should be submitted"
middleChild (TriNode a x y z) = y

-- | rightChild is a function which takes in a trinary tree and returns
-- | the value of the right child, here z, for the given node tree.
-- | It returns an error if input is an emptyNode
rightChild :: TriTree a -> TriTree a
rightChild EmptyNode = error "no empty nodes should be submitted"
rightChild (TriNode a x y z) = z

-- | inTree is a function which takes in two parameters, with the first one being
-- | an element to search for, and the second being the area to search within.
-- | if the value is anywhere within the given area, it returns true,
-- | while if it is not, it returns false instead.
inTree :: Eq a => a -> TriTree a -> Bool
inTree a EmptyNode = False
inTree a (TriNode n x y z)
  | a == n = True
  | inTree a x == True || inTree a y == True || inTree a z == True = True
  | otherwise = False

-- | leaflist is a function which takes in a trinary tree and returns a list of
-- | the values of each leaf(node with three empty branches),
-- | while ignoring anything that is not a leaf
leafList :: TriTree a -> [a]
leafList thetree = case thetree of
  EmptyNode               -> []
  (TriNode a EmptyNode EmptyNode EmptyNode)       -> a:[]
  (TriNode _ x y z)       -> leafList x ++ leafList y ++ leafList z
-- leafList based on my understanding from
-- https://stackoverflow.com/questions/46564896/how-to-return-trees-leaves-in-a-list-in-haskell

-- | inOrderMap is a function which takes in two parameters, with the first one being
-- | a function to run on the tree, and the second being tree to be affected.
-- | the function acts like map on a list, aplying itself to every relevant part,
-- | and in the end a tree is returned that has been transformed by that function,
-- | but is otherwise the same as the input tree.
inOrderMap :: (a -> b) -> TriTree a -> TriTree b
inOrderMap q EmptyNode = EmptyNode
inOrderMap q (TriNode a EmptyNode EmptyNode EmptyNode) = TriNode (q a) EmptyNode EmptyNode EmptyNode
inOrderMap q (TriNode a x y z) = TriNode (q a) (inOrderMap q x) (inOrderMap q y) (inOrderMap q z)

-- | preOrderFold is a function which takes in three parameters,
-- | with the first one being a function, the second being an accumulator value,
-- | and the third being a tritree to pre order walk through.
-- | the function pre order walks through the tree,
-- | applying the function to each value, and the accumulated value so far and then uses the result of that function
-- | in the next call of the folding in the tree. finally, a value is returned,
-- | similar to fold for list, from the value of the accumulator being applied to
-- | each part of the tree.
preOrderFold:: (b -> a -> b) -> b -> TriTree a -> b
preOrderFold func n EmptyNode = n
preOrderFold func n (TriNode a EmptyNode EmptyNode EmptyNode) = (func n a)
preOrderFold func n (TriNode a x y z) = (preOrderFold func (preOrderFold func (preOrderFold func (func n a) x) y) z)

-- preOrderFold fun n (TriNode a x y z) = (fun n a) fun (preOrderFold fun (fun n a) x) fun (preOrderFold fun (preOrderFold fun (fun n a) x) y) fun (preOrderFold fun (preOrderFold fun (preOrderFold fun (fun n a) x) y) z)

-- preOrderFold q n (TriNode a x y z) = (n -> q n a) q (preOrderFold q (q n a) x) q (preOrderFold q n y) q (preOrderFold q n z)

  -- TriTree as a -> a
-- / Credit to Max Hamburg for helping me figure this out

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

-- let TriTree p = TriNode 3 (EmptyNode) (EmptyNode) (EmptyNode)

nodeValue :: TriTree a -> a
nodeValue EmptyNode = error "no empty nodes"
nodeValue (TriNode a x y z) = a

  -- TriTree as a -> a
-- / Credit to Max Hamburg for helping me figure this out

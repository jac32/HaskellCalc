module Bst where 
{-| Represents structure of a Binary Search Tree
Each node is either empty or is a subtree with 2 children
which are valid trees
-}
data Tree a = Empty | Tree a (Tree a) (Tree a)
  deriving Show


{-| Inserts a node into the tree
Does a traversal and compares the value of the node to be inserted against
the nodes it traverses to find the insertion point
-}
insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Tree x Empty Empty
insert x (Tree node l r) = case compare x node of
  EQ -> undefined
  GT -> Tree node l (insert x r)
  LT -> Tree node (insert x l) r

  
{-| Removes a specified node from the tree 
Traverses the tree to find the node to be removed
If there is an empty subtree then the removal is simple
Otherwise delete is called on the root to be removed
-}
remove :: Ord a => a -> Tree a -> Tree a
remove x Empty = undefined
remove x (Tree node l r) = case compare x node of
  EQ -> delete (Tree node l r)
  GT -> Tree node l (remove x r)
  LT -> Tree node (remove x l) r

{-| Returns the tree with the root removed
In the case of two non-empty sub-trees the root is swapped
with the smallest node in the right sub-tree
-}
delete :: Ord a => Tree a -> Tree a
delete (Tree a Empty sub) = sub
delete (Tree a sub Empty) = sub
delete (Tree a sub1 sub2)
  = Tree small sub1 (remove small sub2)
  where small = smallestNode sub2

{-| Returns the smallest node in the given tree
This is useful for deleting roots like in 'delete'
-}
smallestNode :: Ord a => Tree a -> a
smallestNode (Tree a Empty _) = a
smallestNode (Tree _ sub _) = smallestNode sub

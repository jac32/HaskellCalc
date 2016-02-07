module BST where 
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
insert :: Ord n => (n,v) -> Tree (n,v) -> Tree (n,v)
insert (n,v) Empty = Tree (n,v) Empty Empty
insert (n,v) (Tree node l r) = case compare n (fst node) of
  EQ -> Tree (n,v) l r
  LT -> Tree node (insert (n,v) l) r
  GT -> Tree node l (insert (n,v) r)

valOf :: Ord n => n -> Tree (n,v) -> Either String v
valOf n Empty = Left "Value not found in tree"
valOf n (Tree node l r) = case compare n (fst node) of
  EQ -> Right (snd node)
  LT -> valOf n l
  GT -> valOf n r
  

    
{-| Removes a specified node from the tree 
Traverses the tree to find the node to be removed
If there is an empty subtree then the removal is simple
Otherwise delete is called on the root to be removed
-}

remove :: Ord n => n -> Tree (n,v) -> Tree (n,v)
remove n Empty = undefined
remove n (Tree node l r) = case compare n (fst node) of
  EQ -> delete (Tree node l r)
  LT -> Tree node (remove n l) r
  GT -> Tree node l (remove n r)

{-| Returns the tree with the root removed
In the case of two non-empty sub-trees the root is swapped
with the smallest node in the right sub-tree
-}
delete :: Ord n => Tree (n,v) -> Tree (n,v)
delete (Tree node l Empty) = l
delete (Tree node Empty r) = r
delete (Tree small l r)
  = Tree small l (remove (fst small) r)
  where small = smallestNode r
        
{-| Returns the smallest node in the given tree
This is useful for deleting roots like in 'delete'
-}
smallestNode :: Tree (n,v) -> (n,v)
smallestNode (Tree node Empty _) =  node
smallestNode (Tree _ l _) = smallestNode l

{-| Checks a tree for the presence of the given value
True if tree contains a node of the given value
otherwise False
-}
contains :: Ord n => n -> Tree (n,v) -> Bool
contains n Empty = False
contains n (Tree node l r) = case compare n (fst node) of
  EQ -> True
  LT -> (contains n l)
  GT -> (contains n r)

import Test.HUnit
import Struct.BST
import Struct.Value

retrievalTests = TestList $ map TestCase [
  (assertEqual "Retrieval <= empty tree"
   ((Left "Value not found in tree") :: Either String Value)
   (valOf "x" Empty)),
  
  (assertEqual "Retrieval <= single node tree"
   ((Right (I 2)) :: Either String Value)
   (valOf "x" (Tree ("x",(I 2)) Empty Empty))),

  (assertEqual "Retrieval <= complex multi-node tree"
   ((Right (I 2)) :: Either String Value)
   (valOf "a" complexTree))
  ]

insertTests = TestList $ map TestCase [
  (assertEqual "Insertion => empty tree"
   (Tree ("x",(I 2)) Empty Empty)
   (insert ("x",(I 2)) Empty)),

  (assertEqual "Insertion => Tree with lesser name"
   (Tree ("a",(I 99)) Empty (Tree ("b",(I 21)) Empty Empty))
   (insert ("b",(I 21)) (Tree ("a",(I 99)) Empty Empty))),

  (assertEqual "Insertion => Tree with greater name"
   (Tree ("z",(I 99)) (Tree ("b", (I 21)) Empty Empty) Empty)
   (insert ("b",(I 21)) (Tree ("z",(I 99)) Empty Empty))),

  (assertEqual "Insertion => Tree with existing name"
   (Tree ("a",3) Empty Empty)
   (insert ("a",3) (Tree ("a",2) Empty Empty)))
  ]

removalTests = TestList $ map TestCase [
  (assertEqual "Removal of single item"
   (Empty)
   (remove "x" (Tree ("x", (I 2)) Empty Empty)))
  ]

smallestNodeTests = TestList $ map TestCase [
  (assertEqual "Smallest <= two value tree"
   ("a",(I 99))
   (smallestNode (Tree ("a",(I 99)) Empty (Tree ("b",(I 21)) Empty Empty)))),
  
  (assertEqual "Smallest <= complex tree"
   ("a",(I 2))
   (smallestNode complexTree))
 {- (assertEqual "Smallest <= empty tree"
   ("a",(I 2))
   (smallestNode Empty))-}
  ]

bstTests = TestList [insertTests, retrievalTests, removalTests, smallestNodeTests]

----------------------------------------------------------------------------------------
-- Test Data
----------------------------------------------------------------------------------------

{-
         m,26
        /    \
      l,24   n,25
     /   
   b,23
  /   \
 a,2  c,22

-}
complexTree = Tree ("m",(I 26)) (Tree ("l",(I 24)) (Tree ("b",(I 23)) (Tree ("a",(I 2)) Empty Empty) (Tree ("c",(I 22)) Empty Empty)) Empty) (Tree ("n",(I 25)) Empty Empty)

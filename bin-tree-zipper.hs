data Tree a = Leaf a
             | Bin (Tree a) (Tree a)

-- What is a zipper here?

data Branch a = TurnLeft a (Tree a)
               | TurnRight a (Tree a)
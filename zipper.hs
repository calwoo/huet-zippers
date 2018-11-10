module Zipper where

-- Suppose we wanted to build a maze. We decorate each part of the maze with an element.

data Node a = DeadEnd a
    | Passage a (Node a)
    | Fork a (Node a) (Node a)

-- Helper functions to get value/put value.

get :: Node a -> a
get (DeadEnd x) = x
get (Passage x _) = x
get (Fork x _ _) = x

put :: Node a -> a -> Node a
put (DeadEnd _) x = DeadEnd x
put (Passage _ n) x = Passage x n
put (Fork _ n1 n2) x = Fork x n1 n2

-- Let's put a test maze we can play with. We will decorate nodes with coordinates.
labyrinth :: Node (Int, Int)
labyrinth = Fork (0,2)
                (Fork (-2,0) 
                    (DeadEnd (0,-2))
                    (DeadEnd (-1,0)))
                (Passage (2,0)
                    (Fork (1,0)
                        (Passage (0,1)
                            (DeadEnd (0,0)))
                        (DeadEnd (0,-1))))

-- To track our path along the labyrinth, we use "Adriana's thread".
data Branch = KeepStraightOn
            | TurnLeft
            | TurnRight
type Thread = [Branch]

-- To turn down passages and get items, we can for instance, use Thread.
turnRight' :: Thread -> Thread
turnRight' t = t ++ [TurnRight]

retrieve' :: Thread -> Node a -> a
retrieve' [] = get
retrieve' (KeepStraightOn:bs) (Passage _ n) = retrieve bs n
retrieve' (TurnLeft:bs) (Fork _ l r) = retrieve bs l
retrieve' (TurnRight:bs) (Fork _ l r) = retrieve bs r

update' :: (a -> a) -> Thread -> Node a -> Node a
update' f [] n = case n of
                    DeadEnd a -> DeadEnd (f a)
                    Passage a n -> Passage (f a) n
                    Fork a l r -> Fork (f a) l r
update' f (KeepStraightOn:bs) (Passage _ n) = update' f bs n
update' f (TurnLeft:bs) (Fork _ l r) = update' f bs l
update' f (TurnRight:bs) (Fork _ l r) = update' f bs r

-- However, this is a pretty bad solution. Everytime we need to do any action, we need to traverse the
-- entire thread. Even worse, to extend a path or go backwards in a path requires the same traversal.
-- Can we do this another way?

-- Huet's zippers save us!




module Zipper2 where

-- As before, we have

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

-- Test maze.
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

-- But now we use the zipper! A zipper in this case is a data structure that remembers an "augmented thread"
-- and the current sub-maze we are traversing.
type Zipper a = (Thread a, Node a)

-- The thread can be viewed as an environment where the current sub-maze lives. To defined the augmented threads,
-- we note that we define it as a normal thread, but now we also store the "unchosen path" as part of the
-- type data.
data Branch a = KeepStraightOn a
              | TurnLeft a (Node a)
              | TurnRight a (Node a)
type Thread a = [Branch a]

-- Changing positions in the zipper isn't difficult once we know what it should look like geometrically.
turnRight :: Zipper a -> Maybe (Zipper a)
turnRight (t, Fork x l r) = Just (TurnRight x l : t, r)
turnRight _ = Nothing

keepStraightOn :: Zipper a -> Maybe (Zipper a)
keepStraightOn (t, Passage x n) = Just (KeepStraightOn x : t, n)
keepStraightOn _ = Nothing

turnLeft :: Zipper a -> Maybe (Zipper a)
turnLeft (t, Fork x l r) = Just (TurnLeft x r : t, l)
turnLeft _ = Nothing

-- This isn't what we came here for. We want to be able to go back in constant time.
goBack :: Zipper a -> Maybe (Zipper a)
goBack ([], _) = Nothing
goBack (KeepStraightOn x : t, n) = Just (t, Passage x n)
goBack (TurnLeft x r : t, l) = Just (t, Fork x l r)
goBack (TurnRight x l : t, r) = Just (t, Fork x l r)

-- Ahh... sweet bliss.


